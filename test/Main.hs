{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
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
    [ testGroup "ccdb" $
        let
          -- Test indexing the jumpy-jump eventlog.
          testCCDBIndexJumpyJumpWith tableFormat =
            TestIndexOptions
              { executable = "ccdb"
              , eventlog = dataDir </> "jumpy-jump.eventlog.gz"
              , numEntries = 277
              , checkEntries =
                  [
                    ( "0x2"
                    , "0x2: Just (CostCentre {\
                      \ccLabel = \"jumpyJump0\", \
                      \ccModule = \"Main\", \
                      \ccSrcLoc = SrcLoc {srcFilePath = \"app/Main.hs\", \
                      \srcRange = Just (Range'MultiLine {line = 40, column = 1, endLine = 55, endColumn = 23})}, ccIsCAF = False})\n"
                    )
                  ]
              , ..
              }
         in
          [ testIndexWith (testCCDBIndexJumpyJumpWith "lsm")
          , testIndexWith (testCCDBIndexJumpyJumpWith "tar")
          , testIndexWith (testCCDBIndexJumpyJumpWith "tgz")
          ]
    , testGroup "ipedb" $
        let
          -- Test indexing the oddball eventlog.
          testIpeDBIndexOddbalWith tableFormat =
            TestIndexOptions
              { executable = "ipedb"
              , eventlog = dataDir </> "oddball.eventlog.gz"
              , numEntries = 157003
              , checkEntries =
                  [
                    ( "0x100000000"
                    , "0x100000000: Just (InfoProv {\
                      \ipName = \"I#_Main_1_con_info\", \
                      \ipClosureDesc = 3, \
                      \ipTyDesc = \"Int\", \
                      \ipLabel = \"main\", \
                      \ipModule = \"Main\", \
                      \ipSrcLoc = SrcLoc {\
                      \srcFilePath = \"app/Main.hs\", \
                      \srcRange = Just (Range'MultiLine {line = 13, column = 1, endLine = 16, endColumn = 11})}})\n"
                    )
                  ]
              , ..
              }
          -- Test stability of indexing fibber-1 and fibber-2.
          testIpeDBEqualFibber =
            TestEqualOptions
              { executable = "ipedb"
              , eventlog1 = dataDir </> "fibber-1.eventlog.gz"
              , eventlog2 = dataDir </> "fibber-2.eventlog.gz"
              , numEntries = 92073
              , tableFormat = "tgz"
              }
         in
          [ testIndexWith (testIpeDBIndexOddbalWith "lsm")
          , testIndexWith (testIpeDBIndexOddbalWith "tar")
          , testIndexWith (testIpeDBIndexOddbalWith "tgz")
          , testEqualWith testIpeDBEqualFibber
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

-- | Get the database filename from an eventlog filename.
toDatabasePath :: FilePath -> String -> FilePath
toDatabasePath eventlog = replaceExtensions (takeFileName eventlog)

--------------------------------------------------------------------------------
-- IpeDB Indexer

data TestIndexOptions = TestIndexOptions
  { executable :: String
  , eventlog :: FilePath
  , tableFormat :: String
  , numEntries :: Int
  , checkEntries :: [(String, String)]
  }

instance HasField "testName" TestIndexOptions TestName where
  getField :: TestIndexOptions -> TestName
  getField options = options.executable <> "[" <> toDatabasePath options.eventlog options.tableFormat <> "]"

testIndexWith :: TestIndexOptions -> TestTree
testIndexWith options =
  testCase options.testName $ do
    withSystemTempDirectory ("ipedb-test-" <> options.testName) $ \tempDir -> do
      let databasePath = tempDir </> toDatabasePath options.eventlog options.tableFormat

      -- Create an IpeDB.
      callProcess options.executable ["index", options.eventlog, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> databasePath]
      assertBool ("Missing output " <> databasePath) =<< doesPathExist databasePath

      -- Count the number of entries in the IpeDB.
      entries <- readProcess options.executable ["list", databasePath, "--table-format=" <> options.tableFormat] ""
      assertEqual ("Database " <> databasePath <> " contains wrong number of entries.") options.numEntries (length $ lines entries)

      -- Query a particular entry.
      for_ options.checkEntries $ \(entryKey, entry) -> do
        actualEntry <- readProcess options.executable ["query", databasePath, "--table-format=" <> options.tableFormat, entryKey] ""
        assertEqual ("Database " <> databasePath <> " contains wrong entry for 0x100000000.") entry actualEntry

--------------------------------------------------------------------------------
-- IpeDB Stability

data TestEqualOptions = TestEqualOptions
  { executable :: String
  , eventlog1 :: FilePath
  , eventlog2 :: FilePath
  , numEntries :: Int
  , tableFormat :: String
  }

instance HasField "testName" TestEqualOptions TestName where
  getField :: TestEqualOptions -> TestName
  getField options =
    options.executable
      <> "["
      <> toDatabasePath options.eventlog1 options.tableFormat
      <> "="
      <> toDatabasePath options.eventlog2 options.tableFormat
      <> "]"

testEqualWith :: TestEqualOptions -> TestTree
testEqualWith options =
  testCase options.testName $ do
    withSystemTempDirectory ("ipedb-test-" <> options.testName) $ \tempDir -> do
      let databasePath1 = tempDir </> toDatabasePath options.eventlog1 options.tableFormat
      let databasePath2 = tempDir </> toDatabasePath options.eventlog2 options.tableFormat

      -- Create two databases.
      callProcess options.executable ["index", options.eventlog1, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> databasePath1]
      assertBool ("Missing output " <> databasePath1) =<< doesPathExist databasePath1
      callProcess options.executable ["index", options.eventlog2, "--eventlog-encoding=gzip", "--table-format=" <> options.tableFormat, "--output=" <> databasePath2]
      assertBool ("Missing output " <> databasePath2) =<< doesPathExist databasePath2

      -- Count the number of entries in the first database.
      entries <- readProcess options.executable ["list", databasePath1, "--table-format=" <> options.tableFormat] ""
      assertEqual ("Database " <> databasePath1 <> " contains wrong number of entries.") options.numEntries (length $ lines entries)

      -- Test that the two generated databases are equal.
      callProcess options.executable ["check", databasePath1, databasePath2]

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
