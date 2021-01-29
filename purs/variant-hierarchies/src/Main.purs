module Main where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (SProxy(..), Variant, expand, inj, prj)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Record (delete, merge)
import Type.Row (type (+))

type ExceptionData r = (message :: String | r)
type Exception r = Record (ExceptionData + (child :: Variant r))

databaseException = SProxy :: SProxy "databaseException"
type DatabaseExceptionData r = (server :: String, database :: String | r)
type DatabaseExceptionRow a b = (databaseException :: Record (DatabaseExceptionData + (child :: Variant a)) | b)
type DatabaseException a b = Exception (DatabaseExceptionRow a b)

type NoChild = (noChild :: Unit) 

noChild :: Variant NoChild
noChild = inj (SProxy :: SProxy "noChild") unit

newDatabaseException :: forall a b.
  {message :: String, server :: String, database :: String, child :: Variant a} -> DatabaseException a b
newDatabaseException {message, server, database, child} = 
  { message, child: inj databaseException { server, database, child } }

asDatabaseException :: forall a b. DatabaseException a b -> Maybe (Record (ExceptionData + DatabaseExceptionData + (child :: Variant a)))
asDatabaseException exn = do
  dbExn <- prj databaseException exn.child
  pure { message: exn.message, server: dbExn.server, database: dbExn.database, child: dbExn.child }

asOnlyDatabaseException :: forall a b. DatabaseException a b -> Maybe (Record (ExceptionData + DatabaseExceptionData + ()))
asOnlyDatabaseException exn = delete (SProxy :: SProxy "child") <$> asDatabaseException exn

databaseConnectionException = SProxy :: SProxy "databaseConnectionException"
type DatabaseConnectionExceptionRow a b = (databaseConnectionException :: {child :: Variant a} | b)
type DatabaseConnectionException a b c = DatabaseException (DatabaseConnectionExceptionRow a b) c

newDatabaseConnectionException :: forall a b c.
  {message :: String, server :: String, database :: String, child :: Variant a } -> DatabaseConnectionException a b c
newDatabaseConnectionException {message, server, database, child} = 
  newDatabaseException {message, server, database, child: inj databaseConnectionException {child}}

asDatabaseConnectionException :: forall a b c. 
  DatabaseConnectionException a b c -> Maybe (Record (ExceptionData + DatabaseExceptionData + (child :: Variant a)))
asDatabaseConnectionException exn = do
  dbExn <- asDatabaseException exn
  dbcExn <- prj databaseConnectionException dbExn.child
  pure $ dbExn { child = dbcExn.child }

asOnlyDatabaseConnectionException :: forall a b c. DatabaseConnectionException a b c -> Maybe (Record (ExceptionData + DatabaseExceptionData + ()))
asOnlyDatabaseConnectionException exn = delete (SProxy :: SProxy "child") <$> asDatabaseConnectionException exn


databaseQueryException = SProxy :: SProxy "databaseQueryException"
type DatabaseQueryExceptionData r = (attemptedQuery :: String | r)
type DatabaseQueryExceptionRow a b = (databaseQueryException :: Record (DatabaseQueryExceptionData + (child :: Variant a)) | b)
type DatabaseQueryException a b c = DatabaseException (DatabaseQueryExceptionRow a b) c

newDatabaseQueryException :: forall a b c.
  {message :: String, server :: String, database :: String, attemptedQuery :: String, child :: Variant a} -> DatabaseQueryException a b c
newDatabaseQueryException {message, server, database, attemptedQuery, child} = 
  newDatabaseException {message, server, database, child: inj databaseQueryException {attemptedQuery, child}}
  
asDatabaseQueryException :: forall a b c.
  DatabaseQueryException a b c -> Maybe (Record (ExceptionData + DatabaseExceptionData + DatabaseQueryExceptionData + (child :: Variant a)))
asDatabaseQueryException exn = do
  dbExn <- asDatabaseException exn
  dbqExn <- prj databaseQueryException dbExn.child
  pure $ (merge {attemptedQuery: dbqExn.attemptedQuery} dbExn) { child = dbqExn.child }
  
asOnlyDatabaseQueryException :: forall a b c.
  DatabaseQueryException a b c -> Maybe (Record (ExceptionData + DatabaseExceptionData + DatabaseQueryExceptionData + ()))
asOnlyDatabaseQueryException exn = delete (SProxy :: SProxy "child") <$> asDatabaseQueryException exn

connErr :: forall a b. DatabaseConnectionException NoChild a b
connErr = newDatabaseConnectionException { server: "MSSQL03", database: "testing", message: "couldn't connect", child: expand noChild }

queryErr :: forall a b. DatabaseQueryException NoChild a b
queryErr = newDatabaseQueryException { server: "MSSQL03", database: "testing", attemptedQuery: "SELECT * FROM debug_log", message: "timeout exceeded", child: expand noChild }

errs :: Array (Exception _) 
errs = [connErr, queryErr]

main :: Effect Unit
main = do
  -- logShow queryErr.message
  -- logShow ((case_ # on databaseException _.server) $ queryErr.child)
  -- logShow queryErr.child.child.attemptedQuery

  logShow (errs <#> asOnlyDatabaseConnectionException)
  logShow (errs <#> asOnlyDatabaseQueryException)
  logShow (errs <#> asOnlyDatabaseException)

