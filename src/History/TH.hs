{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module History.TH where
import Database.Persist.Sql
import qualified Database.Persist as P
import qualified Data.Char
import Data.Text
import Data.Time
import qualified Language.Haskell.TH as TH

import Control.Monad.IO.Class (liftIO,)

import History.Types

upperFirst :: Text -> Text
upperFirst t =
  case uncons t of
    Just (a, b) -> cons (Data.Char.toUpper a) b
    Nothing -> t

lowerFirst :: Text -> Text
lowerFirst t =
  case uncons t of
    Just (a, b) -> cons (Data.Char.toLower a) b
    Nothing -> t


mkHistory :: [EntityDef] -> TH.Q [TH.Dec]
mkHistory (e:es) = case "history" `elem` entityAttrs e of
  True -> do
    rest <- mkHistory es

    historyValF <- TH.funD
      (prefixName "historyVal")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "entity")
          , TH.varP (TH.mkName "now")
        ]
        (TH.normalB $
          [e| $historyRecord |])
        []]

    insertF <- TH.funD
      (prefixName "insert")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "record")
        ]
        (TH.normalB $
          [e| do
            now <- liftIO $ getCurrentTime
            P.insert $ record
              (Created now)
              userKey
              (Updated now)
              userKey
              (Deleted False)
          |])
      []]

    insertKeyF <- TH.funD
      (prefixName "insertKey")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "key")
          , TH.varP (TH.mkName "record")
        ]
        (TH.normalB $
          [e| do
            now <- liftIO $ getCurrentTime
            P.insertKey key $ record
              (Created now)
              userKey
              (Updated now)
              userKey
              (Deleted False)
          |])
      []]
    selectListF <- TH.funD
      (prefixName "selectList")
      [TH.clause
        [[p|filters|], [p|options|]]
          (TH.normalB $
            [e| P.selectList
              ([$deletedFieldE P.==. Deleted False] ++ filters)
              options |])
          []]
    selectFirstF <- TH.funD
      (prefixName "selectFirst")
      [TH.clause
        [[p|filters|], [p|options|]]
        (TH.normalB $
          [e| P.selectFirst
            ([$deletedFieldE P.==. Deleted False] ++ filters)
            options |])
        []]
    updateF <- TH.funD
      (prefixName "update")
      [TH.clause
        [[p|userKey|], [p|entity|], [p|updates|]]
        (TH.normalB $
          [e| do
            now <- liftIO $ getCurrentTime
            P.insertKey
              ($(TH.conE (suffixName "HistoryKey")) (entityKey entity) (Created now))
              $ ($(TH.varE (prefixName "historyVal")) userKey entity now)
            P.update
              (entityKey entity)
              (updates ++ [
                $(TH.conE (suffixName "Updated")) P.=. Updated now
                , $(TH.conE (suffixName "UpdatedBy")) P.=. userKey])
          |])
        []]
    deleteF <- TH.funD
      (prefixName "delete")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "entity")
        ]
        (TH.normalB $
          [e| do
            $(TH.varE . TH.mkName $ "update" ++ eName) userKey entity [$deletedFieldE P.=. Deleted True]
          |])
        []]
    restoreF <- TH.funD
      (prefixName "restore")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "historyentity")
        ]
        (TH.normalB $
          [e| do
            entity <- P.getJustEntity ($(TH.varE (TH.mkName . Data.Text.unpack . lowerFirst . Data.Text.pack . TH.nameBase $ suffixName "HistoryModelFk")) (entityVal historyentity))
            now <- liftIO $ getCurrentTime
            P.insertKey
              ($(TH.conE (suffixName "HistoryKey")) (entityKey entity) (Created now))
              $ ($(TH.varE (prefixName "historyVal")) userKey entity now)
            error "TODO: replace values with values from history record"
          |])
        []]
    return $ historyValF : insertF : insertKeyF : selectListF : selectFirstF : updateF : deleteF : restoreF : rest
    where
      eName = Data.Text.unpack $ unHaskellName (entityHaskell e)
      prefixName s = TH.mkName $ s ++ eName
      suffixName s = TH.mkName $ eName ++ s
      deletedFieldE = TH.conE . TH.mkName $ eName ++ "Deleted"

      fieldName entityField =
        (Data.Text.unpack . lowerFirst . unHaskellName .entityHaskell) e
        ++ (Data.Text.unpack . upperFirst . unHaskellName . fieldHaskell) entityField

      mkFieldE entityField = [e|($(TH.varE (TH.mkName . fieldName $ entityField)) (entityVal entity))|]
      fields =
        [[e|(entityKey entity)|]]
        ++ (Prelude.map mkFieldE $ entityFields e)
        ++ [ [e|(Created now)|]
        , [e|userKey|]]
      historyRecord = Prelude.foldl (\ g x -> [| $g $x |]) (TH.conE (TH.mkName $ eName ++ "History")) fields
  False -> mkHistory es
mkHistory [] = return []
