


module Main where

import Data.CacheFile.Internal
import Test.HUnit
import System.Environment
import System.Directory
import System.Posix.Files
import System.IO
import System.IO.Error hiding (try, catch)
import Prelude hiding (catch)
import Control.Exception

import Control.Applicative

main = do args <- getArgs
          let requested = map findTest args >>= ($ tests)
          runTestTT $ if null requested then tests else test requested

findTest _ (TestCase _) = []
findTest name (TestList tests) = tests >>= findTest name
findTest name (TestLabel tname test) | name == tname = [test]
                                     | otherwise    = []

fileWe'reSureExists = "/"
tmpdir = "/tmp"

tests = test [ test_ignoreIOError,
               test_mtime,
               test_newer ]

test_ignoreIOError =
    "ignoreIOError" ~:
      ["ignores IOErrors" ~:
         ignoreIOError (ioError $ userError "shouldn't see this")
         >>= return . maybe True (const False),
       "doesn't ignore not-IOErrors" ~:
         ignoreIOError (return $ 1 / 0)
         >>= return . maybe False (const True) ]

withOlderAndNewer :: FilePath -> String ->
                    (FilePath -> FilePath -> IO a) ->
                    IO a
withOlderAndNewer dir name f =
    do (older, ohandle) <- openTempFile dir name
       (newer, nhandle) <- openTempFile dir name
       setFileTimes older 0 0
       setFileTimes newer 1 1
       finally (f older newer) $ do hClose ohandle
                                    hClose nhandle
                                    removeFile older
                                    removeFile newer

test_mtime =
    "mtime" ~: do mt  <- getModificationTime fileWe'reSureExists
                  Just mt' <- mtime fileWe'reSureExists
                  return $ mt == mt'

test_newer =
    "newer" ~:
      ["is false when no files exist" ~:
         "" `newer` "" >>= return . not,
       "is false when only one file exists" ~:
         ["on the right" ~: "" `newer` fileWe'reSureExists
           >>= return . not,
          "on the left"  ~: fileWe'reSureExists `newer` ""
           >>= return . not ],
       "is true when the the file on the left is more recent" ~:
          withOlderAndNewer tmpdir "cftest-newer.tmp" $ flip newer,
       "is false when the the file on the right is more recent" ~:
          withOlderAndNewer tmpdir "cftest-newer.tmp" newer >>= return . not
      ]

