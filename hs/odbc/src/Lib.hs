module Lib where

import Relude

import Database.ODBC.SQLServer (FromRow, fromRow, connect, query, close, Value(..))
import Data.Time.LocalTime (LocalTime)

data SendoutDailyActuals = SendoutDailyActuals 
  { sendoutID :: Int
  , readingType :: Int
  , forecastPoint :: Int
  , actualDate :: LocalTime
  , sendoutValue :: Double
  , sendoutTimestamp :: LocalTime
  , entryTimestamp :: LocalTime
  }
  deriving Show

{-# COMPLETE Error, Ok #-}
pattern Error a = Left a
pattern Ok a = Right a

fromValueListToSendoutDailyActuals [IntValue sid, IntValue rt, IntValue fp, LocalTimeValue date, DoubleValue sendout, LocalTimeValue soTimestamp, LocalTimeValue entryTime] = 
  Just $ SendoutDailyActuals sid rt fp date sendout soTimestamp entryTime
fromValueListToSendoutDailyActuals _ = Nothing

pattern SDA :: SendoutDailyActuals -> [Value]
pattern SDA record <- (fromValueListToSendoutDailyActuals -> Just record)

instance FromRow SendoutDailyActuals where
  fromRow (SDA record) = Ok record
  fromRow anythingElse = Error $ "unable to parse " <> show anythingElse <> " as a SendoutDailyActuals record"

someFunc :: IO ()
someFunc = do
  conn <-
    connect
      "DRIVER={ODBC Driver 17 for SQL Server};SERVER=mssql03.ad.mea.energy;Database=SystemTestSource;Trusted_Connection=yes"
  rows <- query conn "SELECT TOP 2 * FROM sendout_daily_actuals" :: IO [SendoutDailyActuals]
  traverse_ print rows
  close conn


