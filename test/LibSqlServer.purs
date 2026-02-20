module Test.Sqlite.LibSqlServer where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Promise (Promise)
import Promise.Aff as Promise

foreign import startLibSqlServerImpl :: Effect (Promise String)
foreign import stopLibSqlServerImpl :: Effect (Promise Unit)

startLibSqlServer :: Aff String
startLibSqlServer = startLibSqlServerImpl # Promise.toAffE

stopLibSqlServer :: Aff Unit
stopLibSqlServer = stopLibSqlServerImpl # Promise.toAffE
