{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_InfoFlow (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/oskarhd/.cabal/bin"
libdir     = "/home/oskarhd/.cabal/lib/x86_64-linux-ghc-9.0.2/InfoFlow-0.1.0.0-inplace-InfoFlow"
dynlibdir  = "/home/oskarhd/.cabal/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/oskarhd/.cabal/share/x86_64-linux-ghc-9.0.2/InfoFlow-0.1.0.0"
libexecdir = "/home/oskarhd/.cabal/libexec/x86_64-linux-ghc-9.0.2/InfoFlow-0.1.0.0"
sysconfdir = "/home/oskarhd/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "InfoFlow_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "InfoFlow_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "InfoFlow_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "InfoFlow_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "InfoFlow_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "InfoFlow_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
