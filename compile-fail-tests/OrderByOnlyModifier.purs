-- EXPECT: ORDER BY requires a column name before ASC/DESC
module Test.CompileFail.OrderByOnlyModifier where

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

bad = from usersTable # selectAll # orderBy @"ASC"
