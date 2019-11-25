{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Lib
  ( someFunc
  ) where

import Prelude ()
import Relude

import Network.Socket (withSocketsDo)
import Control.Exception (bracket)

import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query

import Database.Tds.Message

someFunc :: IO ()
someFunc = do
  let 
    info = 
      defaultConnectInfo 
        { connectHost = "GAS-VM-4524"
        , connectPort = "1433"
        , connectDatabase = "SystemTestSource"
        , connectUser = "sa"
        , connectPassword = "can't put that here"
        }

  withSocketsDo $
    bracket (connect info) close $ \conn -> do
      rs <- sql conn "SELECT * FROM KeyedValues" :: IO [KeyedValues] 
      print rs


data KeyedValues = KeyedValues { key :: Text, value :: Text }
  deriving (Show)

instance Row KeyedValues where
  fromListOfRawBytes [m1,m2] [b1,b2] = KeyedValues d1 d2

    where
      !d1 = fromRawBytes (mcdTypeInfo m1) b1
      !d2 = fromRawBytes (mcdTypeInfo m2) b2

      mcdTypeInfo :: MetaColumnData -> TypeInfo
      mcdTypeInfo (MetaColumnData _ _ ti _ _) = ti

      fromListOfRawBytes _ _ = error "fromListOfRawBytes: List length must be 2"
