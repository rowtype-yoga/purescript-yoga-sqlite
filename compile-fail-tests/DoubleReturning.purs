-- EXPECT: Lacks
module Test.CompileFail.DoubleReturning where

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

bad = from usersTable # insert { name: "A", email: "a@b.com" } # returning @"id" # returning @"name"
