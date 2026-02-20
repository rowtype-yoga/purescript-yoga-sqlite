-- EXPECT: Unclosed string literal
module Test.CompileFail.UnclosedStringLiteralJoin where

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

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , user_id :: Int
  , title :: String
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

postsTable :: Proxy PostsTable
postsTable = Proxy

bad = from usersTable # select @"users.name" # innerJoin @"users.id = 'unclosed" postsTable
