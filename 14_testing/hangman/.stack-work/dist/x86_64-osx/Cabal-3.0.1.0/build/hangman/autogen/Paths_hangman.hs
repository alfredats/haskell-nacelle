{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hangman (
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

bindir     = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/bin"
libdir     = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/lib/x86_64-osx-ghc-8.8.4/hangman-0.1.0.0-HRBKYfaMGKb6Qhis6cSygF-hangman"
dynlibdir  = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/share/x86_64-osx-ghc-8.8.4/hangman-0.1.0.0"
libexecdir = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/libexec/x86_64-osx-ghc-8.8.4/hangman-0.1.0.0"
sysconfdir = "/Users/alfredang/Code/carrotsnotfound/haskell-nacelle/14_testing/hangman/.stack-work/install/x86_64-osx/87cfab14c7f441428a92e57f0b485aa15b3a4f0f6a26f00b3973152fb195c278/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hangman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hangman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hangman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hangman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hangman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hangman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
