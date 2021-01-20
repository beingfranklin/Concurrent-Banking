{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_banking (
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

bindir     = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/bin"
libdir     = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/lib/x86_64-osx-ghc-8.8.4/banking-0.1.0.0-7dpcD39ecrzFCYdsbuh8w4"
dynlibdir  = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/share/x86_64-osx-ghc-8.8.4/banking-0.1.0.0"
libexecdir = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/libexec/x86_64-osx-ghc-8.8.4/banking-0.1.0.0"
sysconfdir = "/Users/franklin/Desktop/banking/.stack-work/install/x86_64-osx/fc287ab80b86501da4327c5cc386c1988caf8ba9b16c1b28dd75082b5c303db5/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "banking_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "banking_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "banking_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "banking_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "banking_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "banking_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
