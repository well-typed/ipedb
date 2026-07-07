{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Records
import System.Directory (doesPathExist)
import System.FilePath (takeBaseName, (<.>), (</>))
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
    ]

dataDir :: FilePath
dataDir = "test" </> "data"

data TestOptions = TestOptions
  { eventlog :: FilePath
  , tableFormat :: String
  }

instance HasField "testName" TestOptions TestName where
  getField :: TestOptions -> TestName
  getField options = takeBaseName options.eventlog <.> options.tableFormat

testWith :: TestOptions -> TestTree
testWith options =
  testCase options.testName $ do
    withSystemTempDirectory ("ipedb-test-" <> options.testName) $ \tempDir -> do
      let ipedb = tempDir </> takeBaseName options.eventlog <.> options.tableFormat

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
