-- EXPECT: Nub
module Test.CompileFail.HavingParamDuplicateName where

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

bad = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # where_ @"name = $x"
  # groupBy @"name"
  # having @"COUNT(*) > $x"
