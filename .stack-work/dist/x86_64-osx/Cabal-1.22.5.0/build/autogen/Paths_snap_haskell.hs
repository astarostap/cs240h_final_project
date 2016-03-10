module Paths_snap_haskell (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/abraham_starosta/Documents/CLASSES/CS240H/snap-haskell/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/bin"
libdir     = "/Users/abraham_starosta/Documents/CLASSES/CS240H/snap-haskell/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/lib/x86_64-osx-ghc-7.10.3/snap-haskell-0.1.0.0-Lgd3Rz2BLGaKkSAAd4VWq6"
datadir    = "/Users/abraham_starosta/Documents/CLASSES/CS240H/snap-haskell/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/share/x86_64-osx-ghc-7.10.3/snap-haskell-0.1.0.0"
libexecdir = "/Users/abraham_starosta/Documents/CLASSES/CS240H/snap-haskell/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/libexec"
sysconfdir = "/Users/abraham_starosta/Documents/CLASSES/CS240H/snap-haskell/.stack-work/install/x86_64-osx/lts-4.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "snap_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "snap_haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "snap_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "snap_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "snap_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
