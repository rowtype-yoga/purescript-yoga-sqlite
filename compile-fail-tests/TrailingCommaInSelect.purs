-- EXPECT: Trailing comma in SELECT clause
module Test.CompileFail.TrailingCommaInSelect where

import Prelude
import Data.Maybe (Maybe(..))
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

bad = from usersTable # select @"name, email,"
