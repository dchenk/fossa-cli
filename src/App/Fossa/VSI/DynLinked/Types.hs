module App.Fossa.VSI.DynLinked.Types (
  LinuxPackageManager (..),
  LinuxPackageMetadata (..),
  DynamicDependency (..),
  ResolvedLinuxPackage (..),
) where

import Data.Text (Text)
import Path (Abs, File, Path)

data LinuxPackageManager
  = LinuxPackageManagerDEB
  | LinuxPackageManagerRPM
  | LinuxPackageManagerAPK

-- | These are all opaque blobs, we're not doing any processing on them other than rendering into a @Locator@.
-- For that reason, stick to @Text@.
data LinuxPackageMetadata = LinuxPackageMetadata
  { linuxPackageID :: Text
  , linuxPackageRevision :: Text
  , linuxPackageDistro :: Text
  , linuxPackageDistroRelease :: Text
  , linuxPackageArch :: Text
  , linuxPackageDistroEpoch :: Maybe Text
  }

data ResolvedLinuxPackage = ResolvedLinuxPackage
  { resolvedLinuxPackageManager :: LinuxPackageManager
  , resolvedLinuxPackageMetadata :: LinuxPackageMetadata
  }

data DynamicDependency = DynamicDependency
  { dynamicDependencyDiskPath :: Path Abs File
  , dynamicDependencyResolved :: Maybe ResolvedLinuxPackage
  }
