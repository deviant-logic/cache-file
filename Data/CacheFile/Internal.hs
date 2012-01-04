{-# LANGUAGE ScopedTypeVariables #-}

module Data.CacheFile.Internal where

import Data.Binary
import System.Time
import System.Directory
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

ignoreIOError :: IO a -> IO (Maybe a)
ignoreIOError io = do ea :: Either IOError a <- try io
                      return $ either (const Nothing) Just ea

mtime :: FilePath -> IO (Maybe ClockTime)
mtime path = ignoreIOError $ getModificationTime path

newer :: FilePath -> FilePath -> IO Bool
newer file file' = do fmt  <- mtime file
                      fmt' <- mtime file'
                      return (fmt > fmt')

defaultSuffix :: String
defaultSuffix = ".cached"

cacheFile :: (Binary a, MonadIO m) => (FilePath -> m a) -> FilePath -> m a
cacheFile = cacheFileSuffixed defaultSuffix

cacheFileAs :: (Binary a, MonadIO m) =>
              FilePath -> (FilePath -> m a) -> FilePath -> m a
cacheFileAs file = cacheFileBy $ const (return file)

cacheFilePrefixed :: (Binary a, MonadIO m) =>
                    String -> (FilePath -> m a) -> FilePath -> m a
cacheFilePrefixed pfx = cacheFileBy $ return . (pfx ++)

cacheFileSuffixed :: (Binary a, MonadIO m) =>
                    String -> (FilePath -> m a) -> FilePath -> m a
cacheFileSuffixed sfx = cacheFileBy $ return . (++sfx)

cacheFileBy :: (Binary a, MonadIO m) =>
              (FilePath -> m FilePath) -> (FilePath -> m a) -> FilePath -> m a
cacheFileBy cached reader path =
    do cached_path  <- cached path
       update_cache <- liftIO $ path `newer` cached_path
       the_data <- if update_cache then
                      reader path
                  else liftIO $ decodeFile cached_path
       when update_cache $ liftIO (encodeFile cached_path the_data)
       return the_data

