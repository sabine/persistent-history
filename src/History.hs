{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module History where

import Data.Text(Text())
import Database.Persist.Sql
import GHC.Generics (Generic)

data HistoryOptions = HistoryOptions {
  userModelName :: Text
  , userKeyTypeName :: Text
  , userKeySqlType :: SqlType
  }
  
defaultOptions = HistoryOptions
  "User"
  "UserId"
  SqlInt64

addHistoryDefs :: HistoryOptions -> [EntityDef] -> [EntityDef]
addHistoryDefs opts (e:es) = case "history" `elem` (entityAttrs e) of
  True -> eWithHistory : historyModelForE : addHistoryDefs opts es
  False -> e : addHistoryDefs opts es
  where
    eWithHistory = e { entityFields = entityFields e ++ [
        FieldDef
          (HaskellName "created")
          (DBName "created")
          (FTTypeCon Nothing "Created")
          SqlDayTime
          []
          True
          NoReference
        , FieldDef
          (HaskellName "createdBy")
          (DBName "created_by")
          (FTTypeCon Nothing (userKeyTypeName opts))
          (userKeySqlType opts)
          []
          True
          (ForeignRef (HaskellName (userModelName opts)) (FTTypeCon Nothing (userKeyTypeName opts)))
        , FieldDef
          (HaskellName "updated")
          (DBName "updated")
          (FTTypeCon Nothing "Updated")
          SqlDayTime
          []
          True
          NoReference
        , FieldDef
          (HaskellName "updatedBy")
          (DBName "updated_by")
          (FTTypeCon Nothing (userKeyTypeName opts))
          (userKeySqlType opts)
          []
          True
          (ForeignRef (HaskellName (userModelName opts)) (FTTypeCon Nothing (userKeyTypeName opts)))
        , deletedField
      ]}
    historyModelForE = EntityDef
      (HaskellName $ historyEntityName)
      (DBName $ unDBName (entityDB e) <> "_history")
      (FieldDef
        (HaskellName "Id")
        (DBName "id")
        (FTTypeCon Nothing $ historyEntityName <> "Id")
        (SqlOther "Composite Reference")
        []
        True
        (CompositeRef
          (CompositeDef [
            FieldDef
              (HaskellName $ "modelFk")
              (DBName "model_fk")
              idFTTypeCon
              (SqlOther "SqlType unset for modelFk")
              []
              True
              NoReference
            , FieldDef
              (HaskellName "created")
              (DBName "created")
              (FTTypeCon Nothing "Created")
              (SqlOther "SqlType unset for created")
              []
              True
              NoReference
            ]
            [])
        )
      )
      ["autogenerated_history_model"]
      ([
        FieldDef
          (HaskellName $ "modelFk")
          (DBName "model_fk")
          idFTTypeCon
          (fieldSqlType . entityId $ e)
          []
          True
          idRef]
        ++ entityFields e
        ++ [deletedField
        , FieldDef
          (HaskellName "created")
          (DBName "created")
          (FTTypeCon Nothing "Created")
          SqlDayTime
          []
          True
          NoReference
        , FieldDef
          (HaskellName "createdBy")
          (DBName "created_by")
          (FTTypeCon Nothing (userKeyTypeName opts))
          (userKeySqlType opts)
          []
          True
          (ForeignRef (HaskellName (userModelName opts)) (FTTypeCon Nothing (userKeyTypeName opts)))
      ])
      []
      []
      []
      mempty
      False
    historyEntityName = unHaskellName (entityHaskell e) <> "History"
    idFTTypeCon = FTTypeCon Nothing (unHaskellName (entityHaskell e) <> "Id")
    idRef = (ForeignRef
            (entityHaskell e)
            idFTTypeCon)
    deletedField = FieldDef
          (HaskellName "deleted")
          (DBName "deleted")
          (FTTypeCon Nothing "Deleted")
          SqlBool
          []
          True
          NoReference
addHistoryDefs _ [] = []
