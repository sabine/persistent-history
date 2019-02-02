{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module History.HistorySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import History.TH
import History.Types
import History
import Database.Persist.TH
import Database.Persist

opts = defaultOptions { userModelName = "Person", userKeyTypeName = "PersonId" }

share [
  mkPersist sqlSettings . addHistoryDefs defaultOptions { userModelName = "Person", userKeyTypeName = "PersonId" },
  mkMigrate "migrateAll" . addHistoryDefs defaultOptions { userModelName = "Person", userKeyTypeName = "PersonId" },
  mkHistory] $ [persistLowerCase|
  Person
    name String
    age Int Maybe
    
  BlogPost history
    title String
    authorId PersonId
|]

spec :: Spec
spec = do
  describe "hello" $ do
    -- TODO: implement actual tests

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

    