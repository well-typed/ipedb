{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Records
import System.Directory (doesPathExist)
import System.FilePath (replaceExtensions, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

main :: IO ()
main = do
  defaultMain . testGroup "ipedb" $
    [ testWith TestOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "lsm"}
    , testWith TestOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "tar"}
    , testWith TestOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "tgz"}
    , testEqualWith TestEqualOptions{eventlog1 = dataDir </> "fibber-1.eventlog.gz", eventlog2 = dataDir </> "fibber-2.eventlog.gz", tableFormat = "tgz"}
    ]

dataDir :: FilePath
dataDir = "test" </> "data"

--------------------------------------------------------------------------------
-- Test indexing

data TestOptions = TestOptions
  { eventlog :: FilePath
  , tableFormat :: String
  }

instance HasField "testName" TestOptions TestName where
  getField :: TestOptions -> TestName
  getField options = toIpeDBPath options.eventlog options.tableFormat

testWith :: TestOptions -> TestTree
testWith options =
  testCase options.testName $ do
    withSystemTempDirectory ("ipedb-test-" <> options.testName) $ \tempDir -> do
      let ipedb = tempDir </> toIpeDBPath options.eventlog options.tableFormat

      -- Create an IpeDB.
      callProcess "ipedb" ["index", options.eventlog, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> ipedb]
      assertBool ("Missing output " <> ipedb) =<< doesPathExist ipedb

      -- Count the number of entries in the IpeDB.
      entries <- readProcess "ipedb" ["list", ipedb, "--table-format=" <> options.tableFormat] ""
      assertEqual ("IpeDB " <> ipedb <> " contains wrong number of entries.") 157003 (length $ lines entries)

      -- Query a particular entry.
      let expectedEntry =
            "0x100000000: Just (InfoProv {\
            \ipName = \"I#_Main_1_con_info\", \
            \ipClosureDesc = 3, \
            \ipTyDesc = \"Int\", \
            \ipLabel = \"main\", \
            \ipModule = \"Main\", \
            \ipSrcLoc = SrcLoc {\
            \srcFilePath = Just \"app/Main.hs\", \
            \srcRange = Just (Range'MultiLine {line = 13, column = 1, endLine = 16, endColumn = 11})}})\n"
      actualEntry <- readProcess "ipedb" ["query", ipedb, "--table-format=" <> options.tableFormat, "0x100000000"] ""
      assertEqual ("IpeDB " <> ipedb <> " contains wrong entry for 0x100000000.") expectedEntry actualEntry

--------------------------------------------------------------------------------
-- Test stability

data TestEqualOptions = TestEqualOptions
  { eventlog1 :: FilePath
  , eventlog2 :: FilePath
  , tableFormat :: String
  }

instance HasField "testName" TestEqualOptions TestName where
  getField :: TestEqualOptions -> TestName
  getField options =
    toIpeDBPath options.eventlog1 options.tableFormat
      <> "="
      <> toIpeDBPath options.eventlog2 options.tableFormat

testEqualWith :: TestEqualOptions -> TestTree
testEqualWith options =
  testCase options.testName $ do
    withSystemTempDirectory ("ipedb-test-" <> options.testName) $ \tempDir -> do
      let ipedb1 = tempDir </> toIpeDBPath options.eventlog1 options.tableFormat
      let ipedb2 = tempDir </> toIpeDBPath options.eventlog2 options.tableFormat

      -- Create two IpeDBs.
      callProcess "ipedb" ["index", options.eventlog1, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> ipedb1]
      assertBool ("Missing output " <> ipedb1) =<< doesPathExist ipedb1
      callProcess "ipedb" ["index", options.eventlog2, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> ipedb2]
      assertBool ("Missing output " <> ipedb2) =<< doesPathExist ipedb2

      -- Count the number of entries in the first IpeDB.
      entries <- readProcess "ipedb" ["list", ipedb1, "--table-format=" <> options.tableFormat] ""
      assertEqual ("IpeDB " <> ipedb1 <> " contains wrong number of entries.") 92073 (length $ lines entries)

      -- Test that the two generated IpeDBs are equal.
      callProcess "ipedb" ["check", ipedb1, ipedb2]

--------------------------------------------------------------------------------
-- Internal Helpers

toIpeDBPath :: FilePath -> String -> FilePath
toIpeDBPath eventlog = replaceExtensions (takeFileName eventlog)
