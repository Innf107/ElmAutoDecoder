{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ElmAutoDecoder (
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

bindir     = "/home/prophet/.cabal/bin"
libdir     = "/home/prophet/.cabal/lib/x86_64-linux-ghc-8.0.2/ElmAutoDecoder-0.1.0.0-CcbOxyfIx5rDcEQEZZgG2m"
dynlibdir  = "/home/prophet/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/prophet/.cabal/share/x86_64-linux-ghc-8.0.2/ElmAutoDecoder-0.1.0.0"
libexecdir = "/home/prophet/.cabal/libexec"
sysconfdir = "/home/prophet/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ElmAutoDecoder_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ElmAutoDecoder_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ElmAutoDecoder_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ElmAutoDecoder_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ElmAutoDecoder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ElmAutoDecoder_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
