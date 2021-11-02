module Discovery.Filters (
  AllFilters (..),
  targetFilterParser,
  applyFilters,
  FilterCombination (..),
  FilterMatch (..),
  FilterResult (..),
  apply,
) where

import Data.List ((\\))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Semigroup (sconcat)
import Data.Set qualified as Set
import Data.Set.NonEmpty (nonEmpty, toSet)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Path (Dir, Path, Rel, isProperPrefixOf, parseRelDir)
import Text.Megaparsec (
  MonadParsec (eof, takeWhile1P, try),
  Parsec,
  satisfy,
  some,
  (<|>),
 )
import Text.Megaparsec.Char (alphaNumChar, char)
import Types (BuildTarget (..), FoundTargets (FoundTargets, ProjectWithoutTargets), TargetFilter (..))

data AllFilters = AllFilters
  { legacyFilters :: [TargetFilter]
  , includeFilters :: FilterCombination
  , excludeFilters :: FilterCombination
  }

data FilterCombination = FilterCombination
  { combinedTargets :: [TargetFilter]
  , combinedPaths :: [Path Rel Dir]
  }

-- applyFilters determines if legacy filters are present and if they need to converted to `TargetFilters` for filtering.
applyFilters :: AllFilters -> Text -> Path Rel Dir -> FoundTargets -> Maybe FoundTargets
applyFilters (AllFilters [] onlyFilters excludeFilters) tool dir targets = filterFoundTargets (apply onlyFilters excludeFilters tool dir) targets
applyFilters (AllFilters legacyFilters _ _) tool dir targets = filterFoundTargets (apply (FilterCombination legacyFilters []) (FilterCombination [] []) tool dir) targets

-- finalizeResult combines a FilterResult with the targets that exist in a project and returns the final filtering Result
-- If the return value is Nothing, that means that no targets remain for scanning.
-- The second to last cases ensure that the final included targets are intersectioned with the targets that actually exist for a project.
-- The final case ensures that all excluded targets are removed from a projects final list of targets.
filterFoundTargets :: FilterResult -> FoundTargets -> Maybe FoundTargets
filterFoundTargets ResultNone _ = Nothing
filterFoundTargets _ ProjectWithoutTargets = Just ProjectWithoutTargets
filterFoundTargets ResultAll targets = Just targets
filterFoundTargets (ResultInclude found) (FoundTargets targets) = FoundTargets <$> nonEmpty (Set.intersection (toSet targets) $ Set.fromList $ NE.toList found)
filterFoundTargets (ResultExclude found) (FoundTargets targets) = FoundTargets <$> nonEmpty (Set.difference (toSet targets) $ Set.fromList $ NE.toList found)

-- (buildTargetFilters-only `union` pathFilters-only)
--   `subtract` (buildTargetFilters-exclude `union` pathFilters-exclude)
apply :: FilterCombination -> FilterCombination -> Text -> Path Rel Dir -> FilterResult
apply include exclude buildtool dir =
  dSubtract
    (fromMaybe MatchAll (applyComb include buildtool dir))
    (fromMaybe MatchNone (applyComb exclude buildtool dir))

-- Nothing = "Unknown" -- i.e., there were no filters that matched the buildtool + directories.
applyComb :: FilterCombination -> Text -> Path Rel Dir -> Maybe FilterMatch
applyComb comb buildtool dir =
  buildTargetFiltersResult <> pathFiltersResult
  where
    buildTargetFiltersResult :: Maybe FilterMatch
    buildTargetFiltersResult = foldMap' (\t -> applyTarget t buildtool dir) (combinedTargets comb)

    pathFiltersResult :: Maybe FilterMatch
    pathFiltersResult = foldMap' (`applyPath` dir) (combinedPaths comb)

applyTarget :: TargetFilter -> Text -> Path Rel Dir -> FilterMatch
applyTarget (TypeTarget t) u _ = if t == u then MatchAll else MatchNone
applyTarget (TypeDirTarget t p) u q = if t == u && p == q then MatchAll else MatchNone
applyTarget (TypeDirTargetTarget t p target) u q = if t == u && p == q then MatchSome (target NE.:| []) else MatchNone

-- (parent path) (child path)
applyPath :: Path Rel Dir -> Path Rel Dir -> FilterMatch
applyPath t u = if isProperPrefixOf t u || t == u then MatchAll else MatchNone

-- MatchNone <> MatchAll = MatchAll is the reason for this order
-- (MatchSome <> MatchAll) and (MatchAll <> MatchSome) outputs the results in MatchSome.
-- The implications of this are that if a TypeTargetFilter matches all targets and
-- a TypeDirTargetTargetFilter matches a specific target we will prefer the specific target.
instance Semigroup FilterMatch where
  MatchNone <> t = t
  t <> MatchNone = t
  t <> MatchAll = t
  MatchAll <> t = t
  MatchSome ts <> MatchSome us = MatchSome (ts <> us)

-- | 'foldMap', but only requires a 'Semigroup' instance.
--
-- When the provided list is empty, this returns 'Nothing'
foldMap' :: Semigroup s => (a -> s) -> [a] -> Maybe s
foldMap' f xs = sconcat <$> NE.nonEmpty (map f xs)

data FilterMatch = MatchNone | MatchAll | MatchSome (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

data FilterResult = ResultNone | ResultAll | ResultInclude (NE.NonEmpty BuildTarget) | ResultExclude (NE.NonEmpty BuildTarget)
  deriving (Eq, Ord, Show)

-- dSubtract -> IncludeMatches -> ExcludeMatches -> FilterResult
-- dSubtract defines how different types of include and exclude matches are merged to create a FilterResult.
dSubtract :: FilterMatch -> FilterMatch -> FilterResult
dSubtract _ MatchAll = ResultNone
dSubtract MatchNone _ = ResultNone
dSubtract MatchAll MatchNone = ResultAll
dSubtract MatchAll (MatchSome xs) = ResultExclude xs
dSubtract (MatchSome xs) MatchNone = ResultInclude xs
dSubtract (MatchSome xs) (MatchSome ys) = maybe ResultNone ResultInclude (neDifference xs ys)

-- | Compute the difference of two non-empty lists
neDifference :: Eq a => NE.NonEmpty a -> NE.NonEmpty a -> Maybe (NE.NonEmpty a)
neDifference xs ys = NE.nonEmpty (NE.toList xs \\ NE.toList ys)

type Parser = Parsec Void Text

targetFilterParser :: Parser TargetFilter
targetFilterParser = (try targetFilter <|> try projectFilter <|> typeFilter) <* eof
  where
    targetFilter = TypeDirTargetTarget <$> buildtool <* char '@' <*> path <* char ':' <*> target
    projectFilter = TypeDirTarget <$> buildtool <* char '@' <*> path
    typeFilter = TypeTarget <$> buildtool

    buildtool :: Parser Text
    buildtool = toText <$> some alphaNumChar

    path :: Parser (Path Rel Dir)
    path = do
      filepath <- some (satisfy (/= ':'))
      case parseRelDir filepath of
        Left err -> fail (show err)
        Right a -> pure a

    target :: Parser BuildTarget
    target = BuildTarget <$> takeWhile1P Nothing (const True)