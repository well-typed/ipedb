{-# LANGUAGE OverloadedStrings #-}

module IpeDb.Main (defaultMain) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import IpeDb.Eventlog.Index
import IpeDb.InfoProv
import qualified IpeDb.Options as Opts
import IpeDb.Query

defaultMain :: IO ()
defaultMain = do
  opts <- Opts.parseOptions
  case opts.command of
    Opts.Query query -> withInfoProvDb opts.databaseFp $ \db -> do
      lookupInfoProv db query.ipeId >>= \case
        Nothing -> Text.putStrLn $ "No Info Prov found for " <> Text.show query.ipeId
        Just prov -> Text.putStrLn $ prettyInfoProv prov
    Opts.Index index -> withInfoProvDb opts.databaseFp $ \db -> do
      generateInfoProvDb db index.eventlog

prettyInfoProv :: InfoProv -> Text
prettyInfoProv prov =
  Text.unlines
    [ "Info Table  " <> prettyIpeId prov.infoId
    , "  Table Name:      " <> prov.tableName
    , "  Closure Desc:    " <> Text.show prov.closureDesc
    , "  Type Desc:       " <> prov.typeDesc
    , "  Label:           " <> prov.label
    , "  Modulename:      " <> prov.moduleName
    , "  Source Location: " <> prov.srcLoc
    ]
