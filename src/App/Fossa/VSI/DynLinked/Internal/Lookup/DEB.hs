module App.Fossa.VSI.DynLinked.Internal.Lookup.DEB (
  lookupDependencies,
  parseMetaOutput,
) where

import App.Fossa.VSI.DynLinked.Types (DynamicDependency (..), LinuxDistro, LinuxPackageManager (..), LinuxPackageMetadata (..), ResolvedLinuxPackage (..))
import App.Fossa.VSI.DynLinked.Util (runningLinux)
import Control.Algebra (Has)
import Control.Effect.Diagnostics (Diagnostics, recover)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.String.Conversion (toText)
import Data.Text (Text)
import Data.Void (Void)
import Effect.Exec (AllowErr (Never), Command (..), Exec, execParser)
import Path (Abs, Dir, File, Path)
import Text.Megaparsec (Parsec, empty, takeWhile1P)
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | The idea here is that we look up what paths we can with apt and turn them into @DynamicDependency@.
-- We then hand back leftovers and lookup results for the next resolution function.
lookupDependencies :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> LinuxDistro -> [Path Abs File] -> m ([Path Abs File], [DynamicDependency])
lookupDependencies _ _ files | not runningLinux = pure (files, [])
lookupDependencies root distro files = partitionEithers <$> traverse (tryLookup root distro) files

tryLookup :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> LinuxDistro -> Path Abs File -> m (Either (Path Abs File) DynamicDependency)
tryLookup root distro file = fmap (maybeToRight file) . runMaybeT $ do
  name <- MaybeT $ packageForFile root file
  meta <- MaybeT $ packageMeta root name
  pure . DynamicDependency file . Just $ ResolvedLinuxPackage LinuxPackageManagerRPM distro meta

packageForFile :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Path Abs File -> m (Maybe Text)
packageForFile _ _ | not runningLinux = pure Nothing
packageForFile root file = recover . execParser parsePackageForFileOutput root $ packageForFileCommand file

packageForFileCommand :: Path Abs File -> Command
packageForFileCommand file =
  Command
    { cmdName = "dpkg"
    , cmdArgs = ["-S", toText file]
    , cmdAllowErr = Never
    }

-- | Parse @dpkg -S@ output.
-- Example:
--
-- > dpkg -S /lib/x86_64-linux-gnu/libc.so.6
-- > libc6:amd64: /lib/x86_64-linux-gnu/libc.so.6
-- > ^^^^^^^^^^^ we want this part
parsePackageForFileOutput :: Parser Text
parsePackageForFileOutput = ident <* symbol ": "

packageMeta :: (Has Diagnostics sig m, Has Exec sig m) => Path Abs Dir -> Text -> m (Maybe LinuxPackageMetadata)
packageMeta _ _ | not runningLinux = pure Nothing
packageMeta root name = recover . execParser parseMetaOutput root $ packageMetaCommand name

packageMetaCommand :: Text -> Command
packageMetaCommand packageName =
  Command
    { cmdName = "dpkg"
    , cmdArgs = ["-s", packageName]
    , cmdAllowErr = Never
    }

-- | Parse @dpkg -s@ output.
--
-- To keep things simple for now, assume fields always appear in predictable order.
-- if this turns out to be incorrect, we should parse the db directly like syft does.
--
-- Example output:
-- > dpkg -s libc6:amd64
-- > Package: libc6
-- > Status: install ok installed
-- > Priority: optional
-- > Section: libs
-- > Installed-Size: 13246
-- > Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
-- > Architecture: amd64
-- > Multi-Arch: same
-- > Source: glibc
-- > Version: 2.31-0ubuntu9.2
-- > Replaces: libc6-amd64
-- > Depends: libgcc-s1, libcrypt1 (>= 1:4.4.10-10ubuntu4)
-- > Recommends: libidn2-0 (>= 2.0.5~)
-- > Suggests: glibc-doc, debconf | debconf-2.0, locales
-- > Breaks: hurd (<< 1:0.9.git20170910-1), iraf-fitsutil (<< 2018.07.06-4), libtirpc1 (<< 0.2.3), locales (<< 2.31), locales-all (<< 2.31), nocache (<< 1.1-1~), nscd (<< 2.31), r-cran-later (<< 0.7.5+dfsg-2), wcc (<< 0.0.2+dfsg-3)
-- > Conflicts: openrc (<< 0.27-2~)
-- > Conffiles:
-- >  /etc/ld.so.conf.d/x86_64-linux-gnu.conf d4e7a7b88a71b5ffd9e2644e71a0cfab
-- > Description: GNU C Library: Shared libraries
-- >  Contains the standard libraries that are used by nearly all programs on
-- >  the system. This package includes shared versions of the standard C library
-- >  and the standard math library, as well as many others.
-- > Homepage: https://www.gnu.org/software/libc/libc.html
-- > Original-Maintainer: GNU Libc Maintainers <debian-glibc@lists.debian.org>
-- > Original-Vcs-Browser: https://salsa.debian.org/glibc-team/glibc
-- > Original-Vcs-Git: https://salsa.debian.org/glibc-team/glibc.git
parseMetaOutput :: Parser LinuxPackageMetadata
parseMetaOutput = do
  name <- parseField "Package"
  repeatM consumeLine 5
  arch <- parseField "Architecture"
  repeatM consumeLine 2
  version <- parseField "Version"
  pure $ LinuxPackageMetadata name version arch Nothing

consumeLine :: Parser ()
consumeLine = do
  _ <- lexeme $ takeWhile1P Nothing (/= '\n')
  pure ()

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

repeatM :: Monad m => m () -> Word -> m ()
repeatM f n = traverse_ (const f) [1 .. n]
