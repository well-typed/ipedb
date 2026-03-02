{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import IpeDb.InfoProv
import IpeDb.Query qualified as Query
import System.Directory (doesFileExist)
import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

testdataDir :: FilePath
testdataDir = "test" </> "data"

tests :: TestTree
tests =
  testGroup
    "ipedb"
    [ integrationTests
    ]

integrationTests :: TestTree
integrationTests =
  testGroup
    "integration"
    [ testGroup
        "index"
        [ testCase "ipedb.eventlog" $
            withSystemTempDirectory "ipedb" $ \tempDir -> do
              let
                db_loc = tempDir </> "ipedb.db"
                eventlog_log = testdataDir </> "ipedb.eventlog"

              exists_start <- doesFileExist db_loc
              assertBool "Database must not exist before generating it" (not exists_start)

              Query.withInfoProvDb db_loc $ \db ->
                Query.populateFromEventlog db eventlog_log

              exists <- doesFileExist db_loc
              assertBool "Database exists after generating it" exists
        ]
    , testGroup
        "query"
        [ testCase "can insert and find info prov" $
            withSystemTempDirectory "ipedb" $ \tempDir -> do
              let
                db_loc = tempDir </> "ipedb.db"

              Query.withInfoProvDb db_loc $ \db -> do
                Query.setupInfoProvDb db
                let
                  dummyIpe =
                    InfoProv
                      { infoId = IpeId 0xdeadb33f
                      , tableName = "DummyInfo"
                      , closureDesc = 42
                      , typeDesc = "Dummy"
                      , label = "This is a dummy IPE info"
                      , moduleName = "Dummy"
                      , srcLoc = "Dummy.hs:45"
                      }
                Query.insertInfoProv db dummyIpe
                ipe <- Query.lookupInfoProv db dummyIpe.infoId

                ipe @?= Just dummyIpe
        , testCase "list ipedb.eventlog info provs" $
            withSystemTempDirectory "ipedb" $ \tempDir -> do
              let
                db_loc = tempDir </> "ipedb.db"
                eventlog_log = testdataDir </> "ipedb.eventlog"
              Query.withInfoProvDb db_loc $ \db -> do
                Query.populateFromEventlog db eventlog_log
                info_provs <- Query.listInfoProvs db

                assertBool "There must be at least 500 info provs in the table" (length info_provs > 500)
        ]
    ]
