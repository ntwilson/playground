module Main where

import Prelude

import Data.Maybe (Maybe)
import Data.Variant (SProxy(..), Variant, expand, inj, prj)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

type Exception r = { message :: String, child :: Variant r }
type DatabaseExceptionInner r = {server :: String, database :: String, child :: Variant r}
type DatabaseException a b = Exception (databaseException :: DatabaseExceptionInner a | b)
type ExactlyDatabaseException r = Exception (databaseException :: DatabaseExceptionInner r)

type DatabaseConnectionExceptionInner r = { child :: Variant r}
type DatabaseConnectionException a b c = DatabaseException (databaseConnectionException :: DatabaseConnectionExceptionInner a | b) c
type ExactlyDatabaseConnectionException r = ExactlyDatabaseException (databaseConnectionException :: DatabaseConnectionExceptionInner r)

type DatabaseQueryExceptionInner r = {attemptedQuery :: String, child :: Variant r}
type DatabaseQueryException a b c = DatabaseException (databaseQueryException :: DatabaseQueryExceptionInner a | b) c
type ExactlyDatabaseQueryException r = ExactlyDatabaseException (databaseQueryException :: DatabaseQueryExceptionInner r)

type NoChild = (noChild :: Unit) 

noChild :: Variant NoChild
noChild = inj (SProxy :: SProxy "noChild") unit

databaseException = SProxy :: SProxy "databaseException"
newDatabaseException :: forall a b.
  {message :: String, server :: String, database :: String, child :: Variant a} -> DatabaseException a b
newDatabaseException {message, server, database, child} = 
  { message, child: inj databaseException { server, database, child } }

databaseConnectionException = SProxy :: SProxy "databaseConnectionException"
newDatabaseConnectionException :: forall a b c.
  {message :: String, server :: String, database :: String, child :: Variant a } -> DatabaseConnectionException a b c
newDatabaseConnectionException {message, server, database, child} = 
  newDatabaseException {message, server, database, child: inj databaseConnectionException {child}}

databaseQueryException = SProxy :: SProxy "databaseQueryException"
newDatabaseQueryException :: forall a b c.
  {message :: String, server :: String, database :: String, attemptedQuery :: String, child :: Variant a} -> DatabaseQueryException a b c
newDatabaseQueryException {message, server, database, attemptedQuery, child} = 
  newDatabaseException {message, server, database, child: inj databaseQueryException {attemptedQuery, child}}

asDatabaseConnectionException :: forall a b c. 
  DatabaseConnectionException a b c -> Maybe (ExactlyDatabaseConnectionException a)
asDatabaseConnectionException exn = do
  dbExn <- prj databaseException exn.child 
  dbcExn <- prj databaseConnectionException dbExn.child
  pure $ newDatabaseConnectionException 
    { message: exn.message, server: dbExn.server, database: dbExn.database, child: dbcExn.child }
  
asDatabaseQueryException :: forall a b c.
  DatabaseQueryException a b c -> Maybe (ExactlyDatabaseQueryException a) 
asDatabaseQueryException exn = do
  dbExn <- prj databaseException exn.child 
  dbqExn <- prj databaseQueryException dbExn.child
  pure $ newDatabaseQueryException 
    { message: exn.message, server: dbExn.server, database: dbExn.database
    , attemptedQuery: dbqExn.attemptedQuery, child: dbqExn.child 
    }


connErr :: forall a b. DatabaseConnectionException NoChild a b
connErr = newDatabaseConnectionException { server: "MSSQL03", database: "testing", message: "couldn't connect", child: expand noChild }

queryErr :: forall a b. DatabaseQueryException NoChild a b
queryErr = newDatabaseQueryException { server: "MSSQL03", database: "testing", attemptedQuery: "SELECT * FROM debug_log", message: "timeout exceeded", child: expand noChild }

-- expandException exn = exn { child = expand exn.child }

main :: Effect Unit
main = do
  -- logShow queryErr.message
  -- logShow ((case_ # on databaseException _.server) $ queryErr.child)
  -- logShow queryErr.child.child.attemptedQuery
  logShow ([connErr, queryErr] <#> asDatabaseConnectionException)
  logShow ([connErr, queryErr] <#> asDatabaseQueryException)
  log "🍝"
