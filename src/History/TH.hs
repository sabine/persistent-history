{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module History.TH where
import Database.Persist.Sql
import qualified Database.Persist as P
import qualified Data.Char
import qualified Data.Text
import Data.Text (Text())
import Data.Time
import qualified Language.Haskell.TH as TH

import Control.Monad.IO.Class (liftIO,)

import History.Types

upperFirst :: Text -> Text
upperFirst t =
  case Data.Text.uncons t of
    Just (a, b) -> Data.Text.cons (Data.Char.toUpper a) b
    Nothing -> t

lowerFirst :: Text -> Text
lowerFirst t =
  case Data.Text.uncons t of
    Just (a, b) -> Data.Text.cons (Data.Char.toLower a) b
    Nothing -> t

prefix :: TH.Name -> Text -> TH.Name
prefix e s = TH.mkName $ Data.Text.unpack s ++ (TH.nameBase e)

suffix :: TH.Name -> Text -> TH.Name
suffix e s = TH.mkName $ (TH.nameBase e) ++ Data.Text.unpack s


-- TODO: refactor for more clarity
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
            $(TH.varE $ prefixName "update") userKey entity [$deletedFieldE P.=. Deleted True]
          |])
        []]
    revertF <- TH.funD
      (prefixName "revert")
      [TH.clause
        [
          TH.varP (TH.mkName "userKey")
          , TH.varP (TH.mkName "historyentity")
        ]
        (TH.normalB $
          [e| do
            entity <- P.getJustEntity ($historyModelFkE (entityVal historyentity))
            $(TH.varE $ prefixName "update") userKey entity $historyRevertUpdate
          |])
        []]
    return $ historyValF : insertF : insertKeyF : selectListF : selectFirstF : updateF : deleteF : revertF : rest
    where
      eName = TH.mkName $ Data.Text.unpack $ unHaskellName (entityHaskell e)
      prefixName s = prefix eName s
      suffixName s = suffix eName s
      
      deletedFieldE = (TH.conE . suffixName) "Deleted"
      historyModelFkE = TH.varE . TH.mkName . Data.Text.unpack . lowerFirst . Data.Text.pack . TH.nameBase $ suffixName "HistoryModelFk"
      
      eHistoryName = suffix eName "History"

      fieldName f =
        (Data.Text.unpack . lowerFirst . unHaskellName .entityHaskell) e
        ++ (Data.Text.unpack . upperFirst) f

      mkFieldE entityField = [e|($(TH.varE (TH.mkName . fieldName $ unHaskellName . fieldHaskell $ entityField)) (entityVal entity))|]
      fields =
        [[e|(entityKey entity)|]]
        ++ (Prelude.map mkFieldE $ entityFields e)
        ++ [ [e|($(TH.varE . TH.mkName . fieldName $ "Deleted") (entityVal entity))|] ]
        ++ [ [e|(Created now)|]
        , [e|userKey|]
        ]
      historyRecord = Prelude.foldl (\ g x -> [| $g $x |]) (TH.conE (suffix eName "History")) fields
      
      historyFieldName fieldName = TH.varE . TH.mkName $
        (Data.Text.unpack . lowerFirst . Data.Text.pack . TH.nameBase) eHistoryName
        ++ (Data.Text.unpack . upperFirst) fieldName
      historyConName fieldName =
        (TH.conE . suffix eName) (upperFirst fieldName)
      entityFieldName :: FieldDef -> Text
      entityFieldName = unHaskellName . fieldHaskell
      mkUpdateE entityField = [e| $(historyConName . entityFieldName $ entityField) P.=. $(historyFieldName . entityFieldName $ entityField) (entityVal historyentity)|]
      historyRevertUpdates = 
        -- when we revert, the revertd record can't be deleted
        [ [e| $(historyConName "Deleted") P.=. Deleted False |] ]
        ++ (Prelude.map mkUpdateE $ entityFields e)
      historyRevertUpdate = 
        Prelude.foldl (\ g x -> [e| $x : $g |]) [e|[]|] historyRevertUpdates
  False -> mkHistory es
mkHistory [] = return []
