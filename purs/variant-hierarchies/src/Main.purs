module Main where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Effect.Class.Console (logShow)
import ObjectClass (NoSubtype, ObjectClass, cast, forgetSubtype, instanceOf, new, newLeaf)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type ExceptionData r = (message :: String | r)
type Exception r = ObjectClass (ExceptionData ()) r

databaseException = Proxy :: Proxy "databaseException"
type DatabaseExceptionData r = (server :: String, database :: String | r)
type DatabaseExceptionRow a b = (databaseException :: ObjectClass (DatabaseExceptionData ()) a | b)
type DatabaseException a b = Exception (DatabaseExceptionRow a b)

newDatabaseException :: ∀ a b. { | ExceptionData + DatabaseExceptionData () } -> DatabaseException (NoSubtype a) b
newDatabaseException = new databaseException <<< newLeaf


databaseConnectionException = Proxy :: Proxy "databaseConnectionException"
type DatabaseConnectionExceptionRow a b = (databaseConnectionException :: ObjectClass () a | b)
type DatabaseConnectionException a b c = DatabaseException (DatabaseConnectionExceptionRow a b) c

newDatabaseConnectionException :: ∀ a b c.
  { | ExceptionData + DatabaseExceptionData () } -> DatabaseConnectionException (NoSubtype a) b c
newDatabaseConnectionException = new databaseException <<< new databaseConnectionException <<< newLeaf 

databaseQueryException = Proxy :: _ "databaseQueryException"
type DatabaseQueryExceptionData r = (attemptedQuery :: String | r)
type DatabaseQueryExceptionRow a b = (databaseQueryException :: ObjectClass (DatabaseQueryExceptionData ()) a | b)
type DatabaseQueryException a b c = DatabaseException (DatabaseQueryExceptionRow a b) c

newDatabaseQueryException :: ∀ a b c.
  { | ExceptionData + DatabaseExceptionData + DatabaseQueryExceptionData () } -> DatabaseQueryException (NoSubtype a) b c
newDatabaseQueryException = new databaseException <<< new databaseQueryException <<< newLeaf
  
asDatabaseQueryException :: forall a b c.
  DatabaseQueryException a b c -> _
asDatabaseQueryException exn = exn # instanceOf databaseException >>= instanceOf databaseQueryException
  
connErr :: ∀ a b c. DatabaseConnectionException (NoSubtype a) b c
connErr = newDatabaseConnectionException { server: "MSSQL03", database: "testing", message: "couldn't connect" }

queryErr :: ∀ a b c. DatabaseQueryException (NoSubtype a) b c
queryErr = newDatabaseQueryException { server: "MSSQL03", database: "testing", attemptedQuery: "SELECT * FROM debug_log", message: "timeout exceeded" }

errs :: Array (Exception _) 
errs = [connErr, queryErr]

main :: Effect Unit
main = do
  let (directQueryErr :: DatabaseQueryException (NoSubtype ()) () ()) = queryErr

  logShow queryErr.message
  logShow (cast databaseException directQueryErr).server
  logShow (directQueryErr # cast databaseException # cast databaseQueryException).attemptedQuery

  logShow (errs # Array.mapMaybe (instanceOf databaseException >=> instanceOf databaseConnectionException) <#> forgetSubtype)
  logShow (errs # Array.mapMaybe (instanceOf databaseException >=> instanceOf databaseQueryException) <#> forgetSubtype)
  logShow (errs # Array.mapMaybe (instanceOf databaseException) <#> forgetSubtype)

