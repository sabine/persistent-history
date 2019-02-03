{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module History.Types where

import Database.Persist.Sql
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text())
import Data.Time (UTCTime())
import GHC.Generics (Generic)

newtype Created = Created UTCTime
  deriving (Show, Read, Eq, Generic, PersistField, PersistFieldSql, Ord)
instance FromJSON Created
instance ToJSON Created

newtype Updated = Updated UTCTime
  deriving (Show, Read, Eq, Generic, PersistField, PersistFieldSql)
instance FromJSON Updated
instance ToJSON Updated

newtype Deleted = Deleted Bool
  deriving (Show, Read, Eq, Generic, PersistField, PersistFieldSql)
instance FromJSON Deleted
instance ToJSON Deleted