{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module History.TH (
  mkHistory
  ) where
  
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

entityName = TH.mkName . Data.Text.unpack . unHaskellName .entityHaskell

conNameE :: TH.Name -> Text -> TH.Q TH.Exp
conNameE n fieldName =
  (TH.conE . suffix n) (upperFirst fieldName)
  
varNameE :: TH.Name -> Text -> TH.Q TH.Exp
varNameE n fieldName =
  (TH.varE . suffix (TH.mkName . Data.Text.unpack . lowerFirst . Data.Text.pack . TH.nameBase $ n)) (upperFirst fieldName)

funName :: TH.Name -> Text -> TH.Name
funName = prefix

funNameE :: TH.Name -> Text -> TH.Q TH.Exp
funNameE n = TH.varE . funName n

mkRecordLookupE :: TH.Name -> TH.ExpQ -> Text -> TH.ExpQ
mkRecordLookupE n entityName fieldName = [e|($(varNameE n fieldName) (entityVal $entityName))|]
  
entityFieldName :: FieldDef -> Text
entityFieldName = unHaskellName . fieldHaskell

mkHistoryValF e = TH.funD
  (funName eName "historyVal")
  [TH.clause
    [[p|userKey|], [p|entity|], [p|now|]]
    (TH.normalB $
      [e| $historyRecord |])
    []]
  where
    eName = entityName e
    eHistoryName = suffix eName "History"
    
    mkFieldE entityField = mkRecordLookupE eName [e|entity|] $ entityFieldName entityField
    
    fields =
      [[e|(entityKey entity)|]]
      ++ (Prelude.map mkFieldE $ entityFields e)
      ++ [ mkRecordLookupE eName [e|entity|] "Deleted"
      , [e|(Created now)|]
      , [e|userKey|]
      ]
    historyRecord = Prelude.foldl (\ g x -> [| $g $x |]) (conNameE eName "History") fields  
    
mkInsertF e = TH.funD
  (funName eName "insert")
  [TH.clause
    [[p|userKey|], [p|record|]]
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
  where
    eName = entityName e
    
mkInsertKeyF e = TH.funD
  (funName eName "insertKey")
  [TH.clause
    [[p|userKey|], [p|key|], [p|record|]]
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
  where
    eName = entityName e
    
mkSelectListF e = TH.funD
  (funName eName "selectList")
  [TH.clause
    [[p|filters|], [p|options|]]
      (TH.normalB $
        [e| P.selectList
          ([$(conNameE eName "Deleted") P.==. Deleted False] ++ filters)
          options |])
      []]
  where
    eName = entityName e
      
mkSelectFirstF e = TH.funD
  (funName eName "selectFirst")
  [TH.clause
    [[p|filters|], [p|options|]]
    (TH.normalB $
      [e| P.selectFirst
        ([$(conNameE eName "Deleted") P.==. Deleted False] ++ filters)
        options |])
    []]
  where
    eName = entityName e
  
mkUpdateF e = TH.funD
  (funName eName "update")
  [TH.clause
    [[p|userKey|], [p|entity|], [p|updates|]]
    (TH.normalB $
      [e| do
        now <- liftIO $ getCurrentTime
        P.insertKey
          ($(conNameE eName "HistoryKey") (entityKey entity) (Created now))
          $ ($(funNameE eName "historyVal") userKey entity now)
        P.update
          (entityKey entity)
          (updates ++ [
            $(conNameE eName "Updated") P.=. Updated now
            , $(conNameE eName "UpdatedBy") P.=. userKey])
      |])
    []]
  where
    eName = entityName e
  
mkDeleteF e = TH.funD
  (funName eName "delete")
  [TH.clause
    [[p|userKey|], [p|entity|]]
    (TH.normalB $
      [e| do
        $(funNameE eName "update") userKey entity [$(conNameE eName "Deleted") P.=. Deleted True]
      |])
    []]
  where
    eName = entityName e
  
mkRevertF e = TH.funD
  (funName eName "revert")
  [TH.clause
    [[p|userKey|], [p|historyentity|]]
    (TH.normalB $
      [e| do
        entity <- P.getJustEntity ($(varNameE eHistoryName "ModelFk") (entityVal historyentity))
        $(TH.varE $ prefix eName "update") userKey entity $historyRevertUpdate
      |])
    []]
  where
    eName = entityName e
    eHistoryName = suffix eName "History"
    
    mkUpdateE entityField = [e|
      $(conNameE eName . entityFieldName $ entityField)
      P.=. $(mkRecordLookupE eHistoryName [e|historyentity|] . entityFieldName $ entityField)|]
    historyRevertUpdates = 
      -- when we revert, the reverted record is automatically undeleted
      [ [e| $(conNameE eName "Deleted") P.=. Deleted False |] ]
      ++ (Prelude.map mkUpdateE $ entityFields e)
    historyRevertUpdate = 
      Prelude.foldl (\ g x -> [e| $x : $g |]) [e|[]|] historyRevertUpdates
  
mkHistory :: [EntityDef] -> TH.Q [TH.Dec]
mkHistory (e:es) = case "history" `elem` entityAttrs e of
  True -> do
    historyValF <- mkHistoryValF e
    insertF <- mkInsertF e
    insertKeyF <- mkInsertKeyF e
    selectListF <- mkSelectListF e
    selectFirstF <- mkSelectFirstF e
    updateF <- mkUpdateF e
    deleteF <- mkDeleteF e
    revertF <- mkRevertF e
    rest <- mkHistory es
    return $
      historyValF 
      : insertF : insertKeyF
      : selectListF : selectFirstF
      : updateF
      : deleteF
      : revertF
      : rest
  False ->
    mkHistory es
mkHistory [] = return []
