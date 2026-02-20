-- EXPECT: NoInstanceFound
module Test.CompileFail.JoinAmbiguousColumn where

import Prelude
import Data.Maybe (Maybe(..))
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.SQLite.Schema

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  )

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , user_id :: Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

postsTable :: Proxy PostsTable
postsTable = Proxy

bad = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"id"
