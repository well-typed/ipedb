{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import GHC.Records
import IpeDB.Types.SrcLoc (Range (..), SrcLoc (..), parseRange, parseSrcLoc)
import System.Directory (doesPathExist)
import System.FilePath (replaceExtensions, takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

main :: IO ()
main = do
  defaultMain . testGroup "Tests" $
    [ testGroup "IpeDB" $
        [ testIndexWith TestIndexOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "lsm"}
        , testIndexWith TestIndexOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "tar"}
        , testIndexWith TestIndexOptions{eventlog = dataDir </> "oddball.eventlog.gz", tableFormat = "tgz"}
        , testEqualWith TestEqualOptions{eventlog1 = dataDir </> "fibber-1.eventlog.gz", eventlog2 = dataDir </> "fibber-2.eventlog.gz", tableFormat = "tgz"}
        ]
    , testGroup "SrcLoc" $
        [ testSrcLoc str maybeSrcLoc
        | (str, maybeSrcLoc) <- srcLocGolden
        ]
    , testGroup "Range" $
        [ testRange str maybeRange
        | (str, maybeRange) <- rangeGolden
        ]
    ]

--------------------------------------------------------------------------------
-- IpeDB
--------------------------------------------------------------------------------

dataDir :: FilePath
dataDir = "test" </> "data"

-- | Get an IpeDB filename from an eventlog filename.
toIpeDBPath :: FilePath -> String -> FilePath
toIpeDBPath eventlog = replaceExtensions (takeFileName eventlog)

--------------------------------------------------------------------------------
-- IpeDB Indexer

data TestIndexOptions = TestIndexOptions
  { eventlog :: FilePath
  , tableFormat :: String
  }

instance HasField "testName" TestIndexOptions TestName where
  getField :: TestIndexOptions -> TestName
  getField options = toIpeDBPath options.eventlog options.tableFormat

testIndexWith :: TestIndexOptions -> TestTree
testIndexWith options =
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
            \srcFilePath = \"app/Main.hs\", \
            \srcRange = Just (Range'MultiLine {line = 13, column = 1, endLine = 16, endColumn = 11})}})\n"
      actualEntry <- readProcess "ipedb" ["query", ipedb, "--table-format=" <> options.tableFormat, "0x100000000"] ""
      assertEqual ("IpeDB " <> ipedb <> " contains wrong entry for 0x100000000.") expectedEntry actualEntry

--------------------------------------------------------------------------------
-- IpeDB Stability

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
-- SrcLoc
--------------------------------------------------------------------------------

srcLocGolden :: [(String, Maybe SrcLoc)]
srcLocGolden =
  [ -- Empty String
    "" |-> Nothing
  , -- Empty SrcLoc
    ":" |-> Just UnhelpfulSrcLoc
  , -- With srcRange = Nothing
    "/path/to/My/Haskell/Module.hs:"
      |-> Just SrcLoc{srcFilePath = "/path/to/My/Haskell/Module.hs", srcRange = Nothing}
  , "C:\\path\\to\\My\\Haskell\\Module.hs:"
      |-> Just SrcLoc{srcFilePath = "C:\\path\\to\\My\\Haskell\\Module.hs", srcRange = Nothing}
  , "/i/am/sneaky/:/::/:::"
      |-> Just SrcLoc{srcFilePath = "/i/am/sneaky/:/::/::", srcRange = Nothing}
  , -- With srcRange = Just Range'Point{line = 1, column = 256}
    "/path/to/My/Haskell/Module.hs:1:256"
      |-> Just SrcLoc{srcFilePath = "/path/to/My/Haskell/Module.hs", srcRange = Just Range'Point{line = 1, column = 256}}
  , "C:\\path\\to\\My\\Haskell\\Module.hs:1:256"
      |-> Just SrcLoc{srcFilePath = "C:\\path\\to\\My\\Haskell\\Module.hs", srcRange = Just Range'Point{line = 1, column = 256}}
  , "/i/am/sneaky/:/::/:::1:256"
      |-> Just SrcLoc{srcFilePath = "/i/am/sneaky/:/::/::", srcRange = Just Range'Point{line = 1, column = 256}}
  , -- With srcRange = Just Range'OneLine {line = 1, column = 3, endColumn = 256}
    "/path/to/My/Haskell/Module.hs:1:3-256"
      |-> Just SrcLoc{srcFilePath = "/path/to/My/Haskell/Module.hs", srcRange = Just Range'OneLine{line = 1, column = 3, endColumn = 256}}
  , "C:\\path\\to\\My\\Haskell\\Module.hs:1:3-256"
      |-> Just SrcLoc{srcFilePath = "C:\\path\\to\\My\\Haskell\\Module.hs", srcRange = Just Range'OneLine{line = 1, column = 3, endColumn = 256}}
  , "/i/am/sneaky/:/::/:::1:3-256"
      |-> Just SrcLoc{srcFilePath = "/i/am/sneaky/:/::/::", srcRange = Just Range'OneLine{line = 1, column = 3, endColumn = 256}}
  , -- With srcRange = Just Range'MultiLine {line = 1, column = 4, endLine = 3, endColumn = 256}
    "/path/to/My/Haskell/Module.hs:(1,4)-(3,256)"
      |-> Just SrcLoc{srcFilePath = "/path/to/My/Haskell/Module.hs", srcRange = Just Range'MultiLine{line = 1, column = 4, endLine = 3, endColumn = 256}}
  , "C:\\path\\to\\My\\Haskell\\Module.hs:(1,4)-(3,256)"
      |-> Just SrcLoc{srcFilePath = "C:\\path\\to\\My\\Haskell\\Module.hs", srcRange = Just Range'MultiLine{line = 1, column = 4, endLine = 3, endColumn = 256}}
  , "/i/am/sneaky/:/::/:::(1,4)-(3,256)"
      |-> Just SrcLoc{srcFilePath = "/i/am/sneaky/:/::/::", srcRange = Just Range'MultiLine{line = 1, column = 4, endLine = 3, endColumn = 256}}
  ]
 where
  (|->) = (,)

testSrcLoc :: String -> Maybe SrcLoc -> TestTree
testSrcLoc str expect =
  testCase ("parseSrcLoc " <> show str) $ do
    let errMsg = "parseSrcLoc returned incorrect result for '" <> str <> "'"
    let actual = either (const Nothing) Just (parseSrcLoc str)
    assertEqual errMsg expect actual

--------------------------------------------------------------------------------
-- Range
--------------------------------------------------------------------------------

rangeGolden :: [(String, Maybe Range)]
rangeGolden =
  [ "" |-> Nothing
  , "1:256" |-> Just Range'Point{line = 1, column = 256}
  , "1:3-256" |-> Just Range'OneLine{line = 1, column = 3, endColumn = 256}
  , "(1,4)-(3,256)" |-> Just Range'MultiLine{line = 1, column = 4, endLine = 3, endColumn = 256}
  ]
 where
  (|->) = (,)

testRange :: String -> Maybe Range -> TestTree
testRange str expect =
  testCase ("parseRange " <> show str) $ do
    let errMsg = "parseRange returned incorrect result for '" <> str <> "'"
    let actual = either (const Nothing) Just (parseRange str)
    assertEqual errMsg expect actual
