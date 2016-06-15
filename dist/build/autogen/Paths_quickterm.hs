{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_quickterm (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/samuel/.cabal/bin"
libdir     = "/home/samuel/.cabal/lib/x86_64-linux-ghc-8.0.1/quickterm-0.1.0.0-3734NpfvX3fBMxiM8OHLuo"
datadir    = "/home/samuel/.cabal/share/x86_64-linux-ghc-8.0.1/quickterm-0.1.0.0"
libexecdir = "/home/samuel/.cabal/libexec"
sysconfdir = "/home/samuel/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quickterm_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quickterm_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "quickterm_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quickterm_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quickterm_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
