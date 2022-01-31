module App.Fossa.VSI.DynLinked.Internal.Lookup.RPM (
  lookupDependencies,
  parseMetaOutput,
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, recover)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Path (Abs, Dir, File, Path)
import Text.Megaparsec (MonadParsec (eof), Parsec, empty, option, takeWhile1P, try)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | The idea here is that we look up what paths we can with RPM and turn them into @DynamicDependency@.
-- We then hand back leftovers and lookup results for the next resolution function.
lookupDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency])
lookupDependencies _ files | not runningLinux = pure (files, [])
lookupDependencies root files = partitionEithers <$> traverse (tryLookup root) files

tryLookup :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Either (Path Abs File) DynamicDependency)
tryLookup root file = fmap (maybeToRight file) . runMaybeT $ do
  name <- MaybeT $ packageForFile root file
  meta <- MaybeT $ packageMeta root name
  pure . DynamicDependency file . Just $ ResolvedLinuxPackage LinuxPackageManagerRPM meta

packageForFile :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Maybe Text)
packageForFile _ _ | not runningLinux = pure Nothing
packageForFile root file = recover . execParser parsePackageForFileOutput root $ packageForFileCommand file

packageForFileCommand :: Path Abs File -> Command
packageForFileCommand file =
  Command
    { cmdName = "rpm"
    , cmdArgs = ["-qf", toText file]
    , cmdAllowErr = Never
    }

-- | Parse @rpm -qf@ output.
-- Example:
--
-- > rpm -qf /lib64/libc.so.6
-- > glibc-2.28-151.el8.x86_64
-- > ^^^^^^^^^^^^^^^^^^^^^^^^^ we want this whole output.
parsePackageForFileOutput :: Parser Text
parsePackageForFileOutput = takeWhile1P Nothing (const True) <* eof

packageMeta :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Text -> m (Maybe LinuxPackageMetadata)
packageMeta _ _ | not runningLinux = pure Nothing
packageMeta root name = recover . execParser parseMetaOutput root $ packageMetaCommand name

packageMetaCommand :: Text -> Command
packageMetaCommand packageName =
  Command
    { cmdName = "rpm"
    , cmdArgs = ["-qi", packageName]
    , cmdAllowErr = Never
    }

-- | Parse @rpm -qi@ output.
--
-- To keep things simple for now, assume fields always appear in predictable order.
-- if this turns out to be incorrect, we should parse the rpm db directly, like syft does.
--
-- Example output:
-- > rpm -qi glibc-2.28-151.el8.x86_64
-- > Name        : glibc
-- > Version     : 2.28
-- > Release     : 151.el8
-- > Architecture: x86_64
-- > Install Date: Wed Sep 15 14:17:28 2021
-- > Group       : Unspecified
-- > Size        : 15646740
-- > License     : LGPLv2+ and LGPLv2+ with exceptions and GPLv2+ and GPLv2+ with exceptions and BSD and Inner-Net and ISC and Public Domain and GFDL
-- > Signature   : RSA/SHA256, Thu Mar 11 21:46:42 2021, Key ID 05b555b38483c65d
-- > Source RPM  : glibc-2.28-151.el8.src.rpm
-- > Build Date  : Thu Mar 11 20:16:40 2021
-- > Build Host  : x86-01.mbox.centos.org
-- > Relocations : (not relocatable)
-- > Packager    : CentOS Buildsys <bugs@centos.org>
-- > Vendor      : CentOS
-- > URL         : http://www.gnu.org/software/glibc/
-- > Summary     : The GNU libc libraries
-- > Description :
-- > The glibc package contains standard libraries which are used by
-- > multiple programs on the system. In order to save disk space and
-- > memory, as well as to make upgrading easier, common system code is
-- > kept in one place and shared between programs. This particular package
-- > contains the most important sets of shared libraries: the standard C
-- > library and the standard math library. Without these two libraries, a
-- > Linux system will not function.
parseMetaOutput :: Parser LinuxPackageMetadata
parseMetaOutput = do
  name <- parseField "Name"
  epoch <- try . option Nothing $ Just <$> parseField "Epoch"
  version <- parseField "Version"
  release <- parseField "Release"
  arch <- parseField "Architecture"
  pure $ LinuxPackageMetadata name (version <> "-" <> release) arch epoch

parseField :: Text -> Parser Text
parseField field = symbol field *> symbol ":" *> ident

-- | Consume spaces.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Run the provided parser, then consume any trailing spaces.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse for the provided symbol, then consume any trailing spaces.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Collect a contiguous list of non-space characters into a @Text@, then consume any trailing spaces.
-- Requires that a space trails the identifier.
ident :: Parser Text
ident = lexeme $ toText <$> takeWhile1P Nothing (not . isSpace)

maybeToRight :: a -> Maybe b -> Either a b
maybeToRight df right = case right of
  Just a -> Right a
  Nothing -> Left df
