{-# language TemplateHaskell #-}

module Strategy.Node.PackageJson
  ( discover
  , buildGraph

  , PackageJson(..)
  ) where

import Prologue

import Control.Carrier.Error.Either
import qualified Data.Map.Strict as M
import DepTypes
import Discovery.Walk
import Effect.Grapher
import Effect.ReadFS
import Graphing (Graphing)
import Types

discover :: HasDiscover sig m => Path Abs Dir -> m ()
discover = walk $ \_ _ files -> do
  case find (\f -> fileName f == "package.json") files of
    Nothing -> pure ()
    Just file -> runSimpleStrategy "nodejs-packagejson" NodejsGroup $ analyze file

  pure (WalkSkipSome [$(mkRelDir "node_modules")])

data PackageJson = PackageJson
  { packageDeps    :: Map Text Text
  , packageDevDeps :: Map Text Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PackageJson where
  parseJSON = withObject "PackageJson" $ \obj ->
    PackageJson <$> obj .:? "dependencies"    .!= M.empty
                <*> obj .:? "devDependencies" .!= M.empty

analyze :: (Has ReadFS sig m, Has (Error ReadFSErr) sig m) => Path Rel File -> m ProjectClosureBody
analyze file = mkProjectClosure file <$> readContentsJson @PackageJson file

mkProjectClosure :: Path Rel File -> PackageJson -> ProjectClosureBody
mkProjectClosure file package = ProjectClosureBody
  { bodyModuleDir    = parent file
  , bodyDependencies = dependencies
  , bodyLicenses     = []
  }
  where
  dependencies = ProjectDependencies
    { dependenciesGraph    = buildGraph package
    , dependenciesOptimal  = NotOptimal
    , dependenciesComplete = NotComplete
    }

-- TODO: decode version constraints
data NodePackage = NodePackage
  { pkgName       :: Text
  , pkgConstraint :: Text
  } deriving (Eq, Ord, Show, Generic)

type NodeGrapher = LabeledGrapher NodePackage NodePackageLabel

newtype NodePackageLabel = NodePackageEnv DepEnvironment
  deriving (Eq, Ord, Show, Generic)

buildGraph :: PackageJson -> Graphing Dependency
buildGraph PackageJson{..} = run . withLabeling toDependency $ do
  _ <- M.traverseWithKey (addDep EnvProduction) packageDeps
  _ <- M.traverseWithKey (addDep EnvDevelopment) packageDevDeps
  pure ()

  where

  addDep :: Has NodeGrapher sig m => DepEnvironment -> Text -> Text -> m ()
  addDep env name constraint = do
    let pkg = NodePackage name constraint
    direct pkg
    label pkg (NodePackageEnv env)

  toDependency :: NodePackage -> Set NodePackageLabel -> Dependency
  toDependency dep = foldr addLabel (start dep)

  addLabel :: NodePackageLabel -> Dependency -> Dependency
  addLabel (NodePackageEnv env) dep =
    dep { dependencyEnvironments = env : dependencyEnvironments dep }

  start :: NodePackage -> Dependency
  start NodePackage{..} = Dependency
    { dependencyType = NodeJSType
    , dependencyName = pkgName
    , dependencyVersion = Just (CCompatible pkgConstraint)
    , dependencyLocations = []
    , dependencyEnvironments = []
    , dependencyTags = M.empty
    }