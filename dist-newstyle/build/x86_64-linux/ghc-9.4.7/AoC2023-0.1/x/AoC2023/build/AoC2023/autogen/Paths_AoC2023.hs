{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_AoC2023 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/wibble/.cabal/bin"
libdir     = "/home/wibble/.cabal/lib/x86_64-linux-ghc-9.4.7/AoC2023-0.1-inplace-AoC2023"
dynlibdir  = "/home/wibble/.cabal/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/wibble/.cabal/share/x86_64-linux-ghc-9.4.7/AoC2023-0.1"
libexecdir = "/home/wibble/.cabal/libexec/x86_64-linux-ghc-9.4.7/AoC2023-0.1"
sysconfdir = "/home/wibble/.cabal/etc"

getBinDir     = catchIO (getEnv "AoC2023_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "AoC2023_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "AoC2023_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "AoC2023_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AoC2023_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AoC2023_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
