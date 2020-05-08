{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ga_haskell (
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

bindir     = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/bin"
libdir     = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/lib/x86_64-osx-ghc-8.0.2/ga-haskell-0.1.0.0"
dynlibdir  = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/share/x86_64-osx-ghc-8.0.2/ga-haskell-0.1.0.0"
libexecdir = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/libexec"
sysconfdir = "/Users/egorpolivcev/Desktop/haskellprj/.stack-work/install/x86_64-osx/d4801cc85bf4bbfef41eb3e0f2feb1b612271970c88bed16cf909db28c367fdc/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ga_haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ga_haskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ga_haskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ga_haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ga_haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ga_haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
