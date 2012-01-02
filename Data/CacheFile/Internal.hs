{-# LANGUAGE ScopedTypeVariables #-}

module Data.CacheFile.Internal where

import Data.Binary
import System.Time
import System.Directory
import Control.Exception
import Control.Monad

ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError io = do ea :: Either IOError a <- try io
                      return $ either (const Nothing) Just ea

mtime :: FilePath -> IO (Maybe ClockTime)
mtime path = ignoreIOError $ getModificationTime path

newer :: FilePath -> FilePath -> IO Bool
newer file file' = do fmt  <- mtime file
                      fmt' <- mtime file'
                      return (fmt > fmt')

cacheFile :: Binary a => (FilePath -> IO a) -> FilePath -> IO a
cacheFile = cacheFileSuffixed ".cached"

cacheFileAs :: Binary a => FilePath -> (FilePath -> IO a) -> FilePath -> IO a
cacheFileAs = cacheFileBy . const

cacheFilePrefixed :: Binary a => String ->
                               (FilePath -> IO a) -> FilePath -> IO a
cacheFilePrefixed = cacheFileBy . (++)

cacheFileSuffixed :: Binary a => String ->
                               (FilePath -> IO a) -> FilePath -> IO a
cacheFileSuffixed = cacheFileBy . flip (++)

cacheFileBy :: Binary a => (FilePath -> FilePath) ->
                         (FilePath -> IO a)     -> FilePath -> IO a
cacheFileBy cached reader path =
    do update_cache <- path `newer` cached path
       the_data <- if update_cache then
                      reader path
                  else decodeFile (cached path)
       when update_cache $ encodeFile (cached path) the_data
       return the_data

