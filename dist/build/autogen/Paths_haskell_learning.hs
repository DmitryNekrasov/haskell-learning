{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_learning (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import Foreign
import Foreign.C
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
version = Version [1,0] []
prefix, bindirrel :: FilePath
prefix        = "C:\\Users\\nekrasov\\AppData\\Roaming\\cabal"
bindirrel     = "bin"

getBinDir :: IO FilePath
getBinDir = getPrefixDirRel bindirrel

getLibDir :: IO FilePath
getLibDir = getPrefixDirRel "x86_64-windows-ghc-8.0.2\\haskell-learning-1.0"

getDynLibDir :: IO FilePath
getDynLibDir = getPrefixDirRel "x86_64-windows-ghc-8.0.2"

getDataDir :: IO FilePath
getDataDir =  catchIO (getEnv "haskell_learning_datadir") (\_ -> getPrefixDirRel "x86_64-windows-ghc-8.0.2\\haskell-learning-1.0")

getLibexecDir :: IO FilePath
getLibexecDir = getPrefixDirRel "haskell-learning-1.0"

getSysconfDir :: IO FilePath
getSysconfDir = getPrefixDirRel "etc"

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getPrefixDirRel :: FilePath -> IO FilePath
getPrefixDirRel dirRel = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return (prefix `joinFileName` dirRel)
          _ | ret < size -> do
              exePath <- peekCWString buf
              let (bindir,_) = splitFileName exePath
              return ((bindir `minusFileName` bindirrel) `joinFileName` dirRel)
            | otherwise  -> try_size (size * 2)

foreign import ccall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32

minusFileName :: FilePath -> String -> FilePath
minusFileName dir ""     = dir
minusFileName dir "."    = dir
minusFileName dir suffix =
  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))

joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir++fname
  | otherwise                  = dir++pathSeparator:fname

splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse (path2++drive), reverse fname)
  where
    (path,drive) = case p of
       (c:':':p') -> (reverse p',[':',c])
       _          -> (reverse p ,"")
    (fname,path1) = break isPathSeparator path
    path2 = case path1 of
      []                           -> "."
      [_]                          -> path1   -- don't remove the trailing slash if 
                                              -- there is only one character
      (c:path') | isPathSeparator c -> path'
      _                             -> path1

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
