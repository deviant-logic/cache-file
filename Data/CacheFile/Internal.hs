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

cacheFile :: (Binary a, MonadIO m) => (FilePath -> m a) -> FilePath -> m a
cacheFile = cacheFileSuffixed ".cached"

cacheFileAs :: (Binary a, MonadIO m) =>
              FilePath -> (FilePath -> m a) -> FilePath -> m a
cacheFileAs = cacheFileBy . const

cacheFilePrefixed :: (Binary a, MonadIO m) =>
                    String -> (FilePath -> m a) -> FilePath -> m a
cacheFilePrefixed = cacheFileBy . (++)

cacheFileSuffixed :: (Binary a, MonadIO m) =>
                    String -> (FilePath -> m a) -> FilePath -> m a
cacheFileSuffixed = cacheFileBy . flip (++)

cacheFileBy :: (Binary a, MonadIO m) =>
              (FilePath -> FilePath) -> (FilePath -> m a) -> FilePath -> m a
cacheFileBy cached reader path =
    do update_cache <- liftIO $ path `newer` cached path
       the_data <- if update_cache then
                      reader path
                  else liftIO $ decodeFile (cached path)
       when update_cache $ liftIO (encodeFile (cached path) the_data)
       return the_data

