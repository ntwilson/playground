{-# LANGUAGE DeriveAnyClass #-}
module Exceptions (BadAssumption (..), ReportableException) where

-- A ReportableException is any exception that would make sense to try/catch at the top level, and 
-- handle the failure by reporting the exception to the user in some way. An example of an Exception
-- that should not be a ReportableException is UserInterrupt, which is thrown whenever the user terminates
-- the program forcibly.  Such a failure should not be caught and reported back to the user
class Exception e => ReportableException e

data BadAssumption = BadAssumption Text deriving (Show, Exception, ReportableException)
