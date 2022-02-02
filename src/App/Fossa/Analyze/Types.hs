module App.Fossa.Analyze.Types (
  AnalyzeProject (..),
  AnalyzeTaskEffs,
  AnalyzeExperimentalPreferences (..),
  DiscoveredProjectScan (..),
  DiscoveredProjectIdentifier (..),
  IncludeAll (..),
  UnpackArchives (..),
  JsonOutput (..),
) where

import App.Fossa.Analyze.Project (ProjectResult)
import Control.Effect.Debug (Debug)
import Control.Effect.Diagnostics (Diagnostics)
import Control.Effect.Lift
import Control.Effect.Reader (Reader)
import Control.Monad.IO.Class (MonadIO)
import Data.Set (Set)
import Data.Text (Text)
import Diag.Result (Result)
import Effect.Exec (Exec)
import Effect.Logger (Logger)
import Effect.ReadFS (ReadFS)
import Path
import Types

newtype AnalyzeExperimentalPreferences = AnalyzeExperimentalPreferences
  {gradleOnlyConfigsAllowed :: Maybe (Set Text)}
  deriving (Show, Eq, Ord)

type AnalyzeTaskEffs sig m =
  ( Has (Lift IO) sig m
  , MonadIO m
  , Has ReadFS sig m
  , Has Exec sig m
  , Has Logger sig m
  , Has Diagnostics sig m
  , Has Debug sig m
  , Has (Reader AnalyzeExperimentalPreferences) sig m
  )

-- CLI flags, for use with 'Data.Flag'
data UnpackArchives = UnpackArchives
data IncludeAll = IncludeAll
data JsonOutput = JsonOutput

data DiscoveredProjectScan
  = SkippedDueToProvidedFilter DiscoveredProjectIdentifier
  | SkippedDueToDefaultProductionFilter DiscoveredProjectIdentifier
  | Scanned DiscoveredProjectIdentifier (Result ProjectResult)

data DiscoveredProjectIdentifier = DiscoveredProjectIdentifier
  { dpiProjectPath :: Path Abs Dir
  , dpiProjectType :: DiscoveredProjectType
  }
  deriving (Eq, Ord)

class AnalyzeProject a where
  analyzeProject :: AnalyzeTaskEffs sig m => FoundTargets -> a -> m DependencyResults
