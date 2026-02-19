module Test.Sqlite.Main where

import Prelude

import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.Time (Time(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prim.Boolean (True)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Test.Sqlite.TempDb (mkTempDbUrl)
import Yoga.SQLite.SQLite as SQLite
import Yoga.SQLite.Schema

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Table type definitions
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String # Unique
  , age :: Maybe Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

type ConfigTable = Table "config"
  ( id :: Int # PrimaryKey # AutoIncrement
  , active :: SQLiteBool # Default True
  , role :: String # Default "user"
  , score :: Int # Default 0
  )

type EventsTable = Table "events"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , metadata :: Json
  , created_at :: DateTime
  )

eventsTable :: Proxy EventsTable
eventsTable = Proxy

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , user_id :: Int # ForeignKey "users" References "id"
  , title :: String
  , body :: String
  )

postsTable :: Proxy PostsTable
postsTable = Proxy

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type annotations prove correctness at compile time
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedSelectAll
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       ()
       _
typedSelectAll = from usersTable # selectAll

typedSelectCols
  :: Q _
       (name :: String, email :: String)
       ()
       _
typedSelectCols = from usersTable # select @"name, email"

typedSelectAlias
  :: Q _
       (name :: String, e :: String)
       ()
       _
typedSelectAlias = from usersTable # select @"name, email AS e"

typedWhere
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (id :: Int)
       _
typedWhere = from usersTable # selectAll # where_ @"id = $id"

typedWhereComplex
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (name :: String, age :: Int)
       _
typedWhereComplex = from usersTable # selectAll # where_ @"name = $name AND age > $age"

typedWhereStringLiteral
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (status :: String)
       _
typedWhereStringLiteral = from usersTable # selectAll # where_ @"name = $status AND email != 'test@example.com'"

typedInsert
  :: Q _ () () _
typedInsert = from usersTable # insert { name: "Alice", email: "alice@example.com", age: Nothing :: Maybe Int }

typedInsertOptional
  :: Q _ () () _
typedInsertOptional = from usersTable # insert { name: "Alice", email: "alice@example.com" }

typedInsertReturning
  :: Q _ (id :: Int, name :: String) () _
typedInsertReturning = from usersTable
  # insert { name: "Alice", email: "alice@example.com" }
  # returning @"id, name"

typedSet
  :: Q _ () () _
typedSet = from usersTable # set { name: "Bob" }

typedSetWhere
  :: Q _ () (id :: Int) _
typedSetWhere = from usersTable # set { name: "Bob" } # where_ @"id = $id"

typedDelete
  :: Q _ () (id :: Int) _
typedDelete = from usersTable # delete # where_ @"id = $id"

typedDeleteReturning
  :: Q _ (name :: String, email :: String) (id :: Int) _
typedDeleteReturning = from usersTable # delete # where_ @"id = $id" # returning @"name, email"

typedOrderBy
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderBy = from usersTable # selectAll # orderBy @"name"

typedOrderByDesc
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderByDesc = from usersTable # selectAll # orderBy @"name DESC, age ASC"

typedLimitOffset
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedLimitOffset = from usersTable # selectAll # orderBy @"name" # limit @"10" # offset @"5"

typedUpsert
  :: Q _ () () _
typedUpsert = from usersTable
  # insert { name: "Alice", email: "alice@example.com", age: Nothing :: Maybe Int }
  # onConflictDoNothing @"email"

typedReturningAll
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) (id :: Int) _
typedReturningAll = from usersTable # delete # where_ @"id = $id" # returningAll

typedSelectDistinct
  :: Q _ (name :: String) () _
typedSelectDistinct = from usersTable # selectDistinct @"name"

typedGroupBy
  :: Q _ (name :: String, cnt :: Int) () _
typedGroupBy = from usersTable # select @"name, COUNT(*) AS cnt" # groupBy @"name"

typedHaving
  :: Q _ (name :: String, cnt :: Int) (minCount :: Int) _
typedHaving = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # groupBy @"name"
  # having @"COUNT(*) > $minCount"

typedLikeWhere
  :: Q _ _ (title :: String) _
typedLikeWhere = from eventsTable # selectAll # where_ @"title LIKE $title"

-- JOIN tests
typedInnerJoin
  :: Q _ (name :: String, title :: String) () _
typedInnerJoin =
  from usersTable
    # innerJoin @"users.id = posts.user_id" postsTable
    # select @"users.name, posts.title"

typedLeftJoin
  :: Q _ (name :: String, title :: Maybe String) () _
typedLeftJoin =
  from usersTable
    # leftJoin @"users.id = posts.user_id" postsTable
    # select @"users.name, posts.title"

-- Set operations
typedUnion
  :: Q _ (name :: String) () _
typedUnion = do
  let q1 = from usersTable # select @"name"
  let q2 = from usersTable # select @"name"
  union q1 q2

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Helper
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

testDateTime :: DateTime
testDateTime = unsafePartial do
  let year = fromJust (toEnum 2025)
  let month = fromJust (toEnum 1)
  let day = fromJust (toEnum 15)
  let h = fromJust (toEnum 12)
  let mi = fromJust (toEnum 0)
  let s = fromJust (toEnum 0)
  let ms = fromJust (toEnum 0)
  DateTime (canonicalDate year month day) (Time h mi s ms)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Spec
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

setupConn :: Aff SQLite.Connection
setupConn = liftEffect $ SQLite.sqlite { url: ":memory:" }

withTempDb :: Aff SQLite.Connection
withTempDb = liftEffect do
  url <- mkTempDbUrl
  SQLite.sqlite { url }

spec :: Spec Unit
spec = before setupConn do
  describe "DDL generation" do
    it "generates SQLite CREATE TABLE" \_ -> do
      let result = createTableDDL @UsersTable
      result `shouldEqual` "CREATE TABLE users (age INTEGER, email TEXT NOT NULL UNIQUE, id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL)"

    it "generates DDL with defaults" \_ -> do
      let result = createTableDDL @ConfigTable
      result `shouldEqual` "CREATE TABLE config (active INTEGER NOT NULL DEFAULT 1, id INTEGER PRIMARY KEY AUTOINCREMENT, role TEXT NOT NULL DEFAULT 'user', score INTEGER NOT NULL DEFAULT 0)"

  describe "SQL builder output" do
    it "SELECT *" \_ -> do
      toSQL typedSelectAll `shouldEqual` "SELECT * FROM users"

    it "SELECT columns" \_ -> do
      toSQL typedSelectCols `shouldEqual` "SELECT name, email FROM users"

    it "SELECT with alias" \_ -> do
      toSQL typedSelectAlias `shouldEqual` "SELECT name, email AS e FROM users"

    it "WHERE clause" \_ -> do
      toSQL typedWhere `shouldEqual` "SELECT * FROM users WHERE id = $id"

    it "complex WHERE" \_ -> do
      toSQL typedWhereComplex `shouldEqual` "SELECT * FROM users WHERE name = $name AND age > $age"

    it "INSERT" \_ -> do
      toSQL typedInsert `shouldEqual` "INSERT INTO users (age, email, name) VALUES (?1, ?2, ?3)"

    it "INSERT RETURNING" \_ -> do
      toSQL typedInsertReturning `shouldEqual` "INSERT INTO users (email, name) VALUES (?1, ?2) RETURNING id, name"

    it "UPDATE SET" \_ -> do
      toSQL typedSet `shouldEqual` "UPDATE users SET name = ?1"

    it "UPDATE SET WHERE" \_ -> do
      toSQL typedSetWhere `shouldEqual` "UPDATE users SET name = ?1 WHERE id = $id"

    it "DELETE WHERE" \_ -> do
      toSQL typedDelete `shouldEqual` "DELETE FROM users WHERE id = $id"

    it "ON CONFLICT DO NOTHING" \_ -> do
      toSQL typedUpsert `shouldEqual` "INSERT INTO users (age, email, name) VALUES (?1, ?2, ?3) ON CONFLICT (email) DO NOTHING"

    it "ORDER BY" \_ -> do
      toSQL typedOrderBy `shouldEqual` "SELECT * FROM users ORDER BY name"

    it "ORDER BY multiple" \_ -> do
      toSQL typedOrderByDesc `shouldEqual` "SELECT * FROM users ORDER BY name DESC, age ASC"

    it "LIMIT OFFSET" \_ -> do
      toSQL typedLimitOffset `shouldEqual` "SELECT * FROM users ORDER BY name LIMIT 10 OFFSET 5"

    it "SELECT DISTINCT" \_ -> do
      toSQL typedSelectDistinct `shouldEqual` "SELECT DISTINCT name FROM users"

    it "GROUP BY with COUNT" \_ -> do
      toSQL typedGroupBy `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users GROUP BY name"

    it "HAVING" \_ -> do
      toSQL typedHaving `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users GROUP BY name HAVING COUNT(*) > $minCount"

    it "INNER JOIN" \_ -> do
      toSQL typedInnerJoin `shouldEqual` "SELECT users.name, posts.title FROM users INNER JOIN posts ON users.id = posts.user_id"

    it "LEFT JOIN" \_ -> do
      toSQL typedLeftJoin `shouldEqual` "SELECT users.name, posts.title FROM users LEFT JOIN posts ON users.id = posts.user_id"

    it "UNION" \_ -> do
      toSQL typedUnion `shouldEqual` "(SELECT name FROM users) UNION (SELECT name FROM users)"

    it "RETURNING ALL" \_ -> do
      toSQL typedReturningAll `shouldEqual` "DELETE FROM users WHERE id = $id RETURNING *"

  describe "Execution against in-memory DB" do
    it "creates table and inserts data" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      let q = from usersTable # insert { name: "Alice", email: "alice@example.com", age: Just 30 }
      runExecute conn {} q # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int }) `shouldEqual`
        [ { id: 1, name: "Alice", email: "alice@example.com", age: Just 30 } ]

    it "inserts and selects with named params" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      let q = from usersTable # insert { name: "Bob", email: "bob@example.com" }
      runExecute conn {} q # void
      rows <- runQuery conn { name: "Bob" } (from usersTable # selectAll # where_ @"name = $name")
      (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int }) `shouldEqual`
        [ { id: 1, name: "Bob", email: "bob@example.com", age: Nothing } ]

    it "updates rows" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      runExecute conn { id: 1 } (from usersTable # set { name: "Updated" } # where_ @"id = $id") # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Updated"]

    it "deletes rows" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      runExecute conn {} (from usersTable # insert { name: "Bob", email: "b@c.com" }) # void
      runExecute conn { id: 1 } (from usersTable # delete # where_ @"id = $id") # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Bob"]

    it "transactions commit" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      runExecuteTx txn {} (from usersTable # insert { name: "TxUser", email: "tx@test.com" }) # void
      SQLite.commit txn
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["TxUser"]
      SQLite.close conn

    it "transactions rollback" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      runExecuteTx txn {} (from usersTable # insert { name: "Ghost", email: "ghost@test.com" }) # void
      SQLite.rollback txn
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` []
      SQLite.close conn

    it "queryOne returns Maybe" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Solo", email: "solo@test.com" }) # void
      result <- runQueryOne conn { name: "Solo" } (from usersTable # selectAll # where_ @"name = $name")
      let name = map (_.name) (result :: Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
      name `shouldEqual` Just "Solo"
      missing <- runQueryOne conn { name: "Nobody" } (from usersTable # selectAll # where_ @"name = $name")
      (missing :: Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int }) `shouldEqual` Nothing

    it "ON CONFLICT DO NOTHING" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      runExecute conn {} (from usersTable # insert { name: "Alice2", email: "a@b.com" } # onConflictDoNothing @"email") # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice"]

    it "INNER JOIN execution" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      SQLite.executeSimple (SQLite.SQL (createTableDDL @PostsTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      SQLite.execute (SQLite.SQL "INSERT INTO posts (user_id, title, body) VALUES (?1, ?2, ?3)") (map SQLite.toSQLiteValue ["1", "Hello", "World"]) conn # void
      rows <- runQuery conn {}
        ( from usersTable
            # innerJoin @"users.id = posts.user_id" postsTable
            # select @"users.name, posts.title"
        )
      (rows :: Array { name :: String, title :: String }) `shouldEqual`
        [ { name: "Alice", title: "Hello" } ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec
