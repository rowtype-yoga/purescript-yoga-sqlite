-- EXPECT: Expected PARTITION BY or ORDER BY inside OVER clause
module Test.CompileFail.OverClauseInvalidContent where

import Prelude
import Data.Maybe (Maybe)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.SQLite.Schema

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String # Unique
  , age :: Maybe Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

bad = from usersTable # select @"name, ROW_NUMBER() OVER (THIS IS TOTALLY INVALID) AS rn"
