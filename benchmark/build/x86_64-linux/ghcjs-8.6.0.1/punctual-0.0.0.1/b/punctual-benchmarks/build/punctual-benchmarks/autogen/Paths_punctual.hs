{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_punctual (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/d0kt0r0/.cabal/bin"
libdir     = "/Users/d0kt0r0/.cabal/lib/x86_64-linux-ghcjs-8.6.0.1-ghc8_6_5/punctual-0.0.0.1-inplace-punctual-benchmarks"
dynlibdir  = "/Users/d0kt0r0/.cabal/lib/x86_64-linux-ghcjs-8.6.0.1-ghc8_6_5"
datadir    = "/Users/d0kt0r0/.cabal/share/x86_64-linux-ghcjs-8.6.0.1-ghc8_6_5/punctual-0.0.0.1"
libexecdir = "/Users/d0kt0r0/.cabal/libexec/x86_64-linux-ghcjs-8.6.0.1-ghc8_6_5/punctual-0.0.0.1"
sysconfdir = "/Users/d0kt0r0/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "punctual_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "punctual_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "punctual_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "punctual_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "punctual_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "punctual_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
