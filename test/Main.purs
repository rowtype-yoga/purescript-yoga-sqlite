module Test.Sqlite.Main where

import Prelude

import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import JS.BigInt as JS.BigInt
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (un)
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Time (Time(..))
import Effect (Effect)
import Data.Time.Duration (Seconds(..), fromDuration)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Prim.Boolean (True)
import Test.Spec (Spec, around, before, describe, it)
import Control.Parallel (parSequence)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, expectError)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Test.Sqlite.TempDb (mkTempDbUrl)
import Yoga.Test.Docker as Docker
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

type DocumentsTable = Table "documents"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , body :: String
  , embedding :: F32Vector "3"
  )

documentsTable :: Proxy DocumentsTable
documentsTable = Proxy

type ArticlesTable = Table "articles"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , body :: String
  )

articlesTable :: Proxy ArticlesTable
articlesTable = Proxy

type VDocsTable = Table "vdocs"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , emb :: F32Vector "3"
  )

vDocsTable :: Proxy VDocsTable
vDocsTable = Proxy

type F64DocsTable = Table "f64docs"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , embedding :: F64Vector "3"
  )

f64DocsTable :: Proxy F64DocsTable
f64DocsTable = Proxy

type RandomRowIdTable = Table "things"
  ( id :: Int # PrimaryKey # RandomRowId
  , name :: String
  )

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

-- F32Vector / FTS compile-time assertions
typedDocumentsSelectAll
  :: Q _
       (body :: String, embedding :: F32Vector "3", id :: Int, title :: String)
       ()
       _
typedDocumentsSelectAll = from documentsTable # selectAll

typedMatchWhere
  :: Q _ _ (query :: String) _
typedMatchWhere = from articlesTable # selectAll # where_ @"body MATCH $query"

typedMatchTupleWhere
  :: Q _ _ (query :: String) _
typedMatchTupleWhere = from articlesTable # selectAll # where_ @"(title, body) MATCH $query"

typedSelectFtsScore
  :: Q _ (title :: String, score :: Number) (query :: String) _
typedSelectFtsScore =
  from articlesTable
    # select @"title, fts_score(title, body, $query) AS score"
    # where_ @"(title, body) MATCH $query"

typedOrderByRaw
  :: Q _ (title :: String, score :: Number) (query :: String) _
typedOrderByRaw =
  from articlesTable
    # select @"title, fts_score(title, body, $query) AS score"
    # where_ @"(title, body) MATCH $query"
    # orderByRaw @"score DESC"

typedWhereVectorDist
  :: Q _ _ (probe :: F32Vector "3", maxDist :: Number) _
typedWhereVectorDist = from vDocsTable
  # selectAll
  # where_ @"vector_distance_cos(emb, $probe) < $maxDist"

typedWhereVectorDistL2
  :: Q _ _ (probe :: F32Vector "3", maxDist :: Number) _
typedWhereVectorDistL2 = from vDocsTable
  # selectAll
  # where_ @"vector_distance_l2(emb, $probe) < $maxDist"

typedSelectVectorDistL2
  :: Q _ (title :: String, dist :: Number) (probe :: F32Vector "3") _
typedSelectVectorDistL2 = from vDocsTable
  # select @"title, vector_distance_l2(emb, $probe) AS dist"
  # where_ @"vector_distance_l2(emb, $probe) IS NOT NULL"

typedWhereFtsScore
  :: Q _ _ (query :: String, minScore :: Number) _
typedWhereFtsScore = from articlesTable
  # selectAll
  # where_ @"fts_score(title, body, $query) > $minScore"

typedFromRaw
  :: Q _ (title :: String, distance :: Number) (probe :: F32Vector "3", maxDist :: Number) _
typedFromRaw =
  fromRaw @"vector_top_k('idx_emb', $probe, 10) AS top JOIN docs ON docs.rowid = top.id"
    # selectRaw @"docs.title, top.distance" @(title :: String, distance :: Number)
    # whereRaw @"top.distance < $maxDist" @(probe :: F32Vector "3", maxDist :: Number)
    # orderByRaw @"top.distance ASC"

typedWhereRaw
  :: Q _ _ (x :: Int) _
typedWhereRaw = from usersTable # selectAll # whereRaw @"id IN (SELECT id FROM other)" @(x :: Int)

-- F64Vector compile-time: selectAll includes F64Vector column
typedF64DocsSelectAll
  :: Q _
       (embedding :: F64Vector "3", id :: Int, title :: String)
       ()
       _
typedF64DocsSelectAll = from f64DocsTable # selectAll

-- RandomRowId compile-time: insert excludes the id column
typedRandomRowIdInsert
  :: Q _ () () _
typedRandomRowIdInsert = from (Proxy :: Proxy RandomRowIdTable) # insert { name: "test" }

-- vector_distance_l2 in SELECT with alias compile-time
typedSelectDistL2Alias
  :: Q _ (title :: String, l2dist :: Number) (probe :: F32Vector "3") _
typedSelectDistL2Alias = from vDocsTable
  # select @"title, vector_distance_l2(emb, $probe) AS l2dist"
  # where_ @"vector_distance_l2(emb, $probe) IS NOT NULL"

-- VECTOR_DISTANCE_L2 uppercase variant compile-time
typedWhereVectorDistL2Upper
  :: Q _ _ (probe :: F32Vector "3", maxDist :: Number) _
typedWhereVectorDistL2Upper = from vDocsTable
  # selectAll
  # where_ @"VECTOR_DISTANCE_L2(emb, $probe) < $maxDist"

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

    it "WHERE MATCH" \_ -> do
      toSQL typedMatchWhere `shouldEqual` "SELECT * FROM articles WHERE body MATCH $query"

    it "WHERE tuple MATCH" \_ -> do
      toSQL typedMatchTupleWhere `shouldEqual` "SELECT * FROM articles WHERE (title, body) MATCH $query"

    it "select with fts_score" \_ -> do
      toSQL typedSelectFtsScore `shouldEqual` "SELECT title, fts_score(title, body, $query) AS score FROM articles WHERE (title, body) MATCH $query"

    it "orderByRaw" \_ -> do
      toSQL typedOrderByRaw `shouldEqual` "SELECT title, fts_score(title, body, $query) AS score FROM articles WHERE (title, body) MATCH $query ORDER BY score DESC"

    it "fromRaw with vector_top_k" \_ -> do
      toSQL typedFromRaw `shouldEqual` "SELECT docs.title, top.distance FROM vector_top_k('idx_emb', $probe, 10) AS top JOIN docs ON docs.rowid = top.id WHERE top.distance < $maxDist ORDER BY top.distance ASC"

    it "whereRaw" \_ -> do
      toSQL typedWhereRaw `shouldEqual` "SELECT * FROM users WHERE id IN (SELECT id FROM other)"

    it "where_ with vector_distance_cos multi-arg" \_ -> do
      toSQL typedWhereVectorDist `shouldEqual` "SELECT * FROM vdocs WHERE vector_distance_cos(emb, $probe) < $maxDist"

    it "where_ with fts_score multi-arg" \_ -> do
      toSQL typedWhereFtsScore `shouldEqual` "SELECT * FROM articles WHERE fts_score(title, body, $query) > $minScore"

    it "vector32 SQL literal" \_ -> do
      vector32 [1.0, 2.0, 3.0] `shouldEqual` "vector32('[1.0, 2.0, 3.0]')"

  describe "Turso DDL helpers" do
    it "createTableDDL with F32Vector" \_ -> do
      let result = createTableDDL @DocumentsTable
      result `shouldEqual` "CREATE TABLE documents (body TEXT NOT NULL, embedding F32_BLOB(3) NOT NULL, id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL)"

    it "createVectorIndex" \_ -> do
      let result = createVectorIndex @"idx_emb" @"documents" @"embedding" @"metric=cosine"
      result `shouldEqual` "CREATE INDEX idx_emb ON documents (libsql_vector_idx(embedding, 'metric=cosine'))"

    it "createFTSIndex" \_ -> do
      let result = createFTSIndex @"idx_fts" @"articles" @"title, body"
      result `shouldEqual` "CREATE INDEX idx_fts ON articles USING fts (title, body)"

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
      SQLite.close conn # liftEffect

    it "transactions rollback" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      runExecuteTx txn {} (from usersTable # insert { name: "Ghost", email: "ghost@test.com" }) # void
      SQLite.rollback txn
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` []
      SQLite.close conn # liftEffect

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

    it "F32Vector round-trip" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @DocumentsTable)) conn # void
      let vec = f32Vector @"3" [1.0, 2.0, 3.0]
      runExecute conn {} (from documentsTable # insert { title: "test", body: "hello", embedding: vec }) # void
      rows <- runQuery conn {} (from documentsTable # selectAll)
      let result = map (\r -> unF32Vector r.embedding) (rows :: Array { id :: Int, title :: String, body :: String, embedding :: F32Vector "3" })
      result `shouldEqual` [[1.0, 2.0, 3.0]]

    it "F64Vector round-trip" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @F64DocsTable)) conn # void
      let vec = f64Vector @"3" [1.0, 2.0, 3.0]
      runExecute conn {} (from f64DocsTable # insert { title: "test", embedding: vec }) # void
      rows <- runQuery conn {} (from f64DocsTable # selectAll)
      let result = map (\r -> unF64Vector r.embedding) (rows :: Array { id :: Int, title :: String, embedding :: F64Vector "3" })
      result `shouldEqual` [[1.0, 2.0, 3.0]]

    it "columnTypes populated after query" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com", age: Just 30 }) # void
      result <- SQLite.querySimple (SQLite.SQL "SELECT * FROM users") conn
      Array.length result.columnTypes `shouldEqual` Array.length result.columns

    it "batch inserts atomically" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      results <- SQLite.batch SQLite.Write
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Alice", "a@b.com"] }
        , { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Bob", "b@c.com"] }
        ]
        conn
      Array.length results `shouldEqual` 2
      rows <- runQuery conn {} (from usersTable # selectAll # orderBy @"id")
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice", "Bob"]

    it "executeMultiple with multi-statement SQL" \conn -> do
      SQLite.executeMultiple
        ( createTableDDL @UsersTable <> "; INSERT INTO users (name, email) VALUES ('Alice', 'a@b.com'); INSERT INTO users (name, email) VALUES ('Bob', 'b@c.com')"
        )
        conn
      rows <- runQuery conn {} (from usersTable # selectAll # orderBy @"id")
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice", "Bob"]

    it "beginWithMode Read succeeds for read-only queries" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      txn <- SQLite.beginWithMode SQLite.Read conn
      result <- SQLite.txQuery (SQLite.SQL "SELECT * FROM users") [] txn
      Array.length result.rows `shouldEqual` 1
      SQLite.commit txn
      SQLite.close conn # liftEffect

    it "txClose does not throw" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      runExecuteTx txn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      SQLite.commit txn
      SQLite.txClose txn # liftEffect
      SQLite.close conn # liftEffect

    it "txBatch executes multiple statements in transaction" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      results <- SQLite.txBatch
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Alice", "a@b.com"] }
        , { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Bob", "b@c.com"] }
        ]
        txn
      SQLite.commit txn
      Array.length results `shouldEqual` 2
      rows <- runQuery conn {} (from usersTable # selectAll # orderBy @"id")
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice", "Bob"]
      SQLite.close conn # liftEffect

    it "txExecuteMultiple runs multi-statement SQL in transaction" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      SQLite.txExecuteMultiple "INSERT INTO users (name, email) VALUES ('Alice', 'a@b.com'); INSERT INTO users (name, email) VALUES ('Bob', 'b@c.com')" txn
      SQLite.commit txn
      rows <- runQuery conn {} (from usersTable # selectAll # orderBy @"id")
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice", "Bob"]
      SQLite.close conn # liftEffect

    it "closed returns false when open, true after close" \_ -> do
      conn <- withTempDb
      before_ <- SQLite.closed conn # liftEffect
      before_ `shouldEqual` false
      SQLite.close conn # liftEffect
      after_ <- SQLite.closed conn # liftEffect
      after_ `shouldEqual` true

    it "protocol returns file for file-backed DB" \_ -> do
      conn <- withTempDb
      p <- SQLite.protocol conn # liftEffect
      p `shouldEqual` "file"
      SQLite.close conn # liftEffect

    it "empty batch returns empty array" \conn -> do
      results <- SQLite.batch SQLite.Write [] conn
      Array.length results `shouldEqual` 0

    it "txClosed reflects transaction state" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      before_ <- SQLite.txClosed txn # liftEffect
      before_ `shouldEqual` false
      SQLite.commit txn
      after_ <- SQLite.txClosed txn # liftEffect
      after_ `shouldEqual` true
      SQLite.close conn # liftEffect

    it "Maybe SQLiteValue passes null for Nothing" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      SQLite.execute
        (SQLite.SQL "INSERT INTO users (name, email, age) VALUES (?1, ?2, ?3)")
        [ SQLite.toSQLiteValue "Alice"
        , SQLite.toSQLiteValue "a@b.com"
        , SQLite.toSQLiteValue (Nothing :: Maybe Int)
        ]
        conn # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let ages = map (_.age) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      ages `shouldEqual` [Nothing]

    it "SQLiteBool as SQLiteValue" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @ConfigTable)) conn # void
      SQLite.execute
        (SQLite.SQL "INSERT INTO config (active) VALUES (?1)")
        [ SQLite.toSQLiteValue (SQLiteBool false) ]
        conn # void
      rows <- runQuery conn {} (from (Proxy :: Proxy ConfigTable) # selectAll)
      let actives = map (\r -> r.active) (rows :: Array { id :: Int, active :: SQLiteBool, role :: String, score :: Int })
      actives `shouldEqual` [SQLiteBool false]

    it "RandomRowId column excluded from typed insert" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @RandomRowIdTable)) conn # void
      runExecute conn {} (from (Proxy :: Proxy RandomRowIdTable) # insert { name: "hello" }) # void
      rows <- runQuery conn {} (from (Proxy :: Proxy RandomRowIdTable) # select @"name")
      let names = map (_.name) (rows :: Array { name :: String })
      names `shouldEqual` ["hello"]

    it "DateTime as SQLiteValue in batch" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @EventsTable)) conn # void
      SQLite.execute
        (SQLite.SQL "INSERT INTO events (title, metadata, created_at) VALUES (?1, ?2, ?3)")
        [ SQLite.toSQLiteValue "test"
        , SQLite.toSQLiteValue "{}"
        , SQLite.toSQLiteValue testDateTime
        ]
        conn # void
      rows <- runQuery conn {} (from eventsTable # selectAll)
      Array.length rows `shouldEqual` 1

  describe "DDL RANDOM ROWID" do
    it "generates DDL with RANDOM ROWID suffix" \_ -> do
      let result = createTableDDL @RandomRowIdTable
      result `shouldEqual` "CREATE TABLE things (id INTEGER NOT NULL PRIMARY KEY, name TEXT NOT NULL) RANDOM ROWID"

  describe "DDL F64Vector" do
    it "createTableDDL with F64Vector" \_ -> do
      let result = createTableDDL @F64DocsTable
      result `shouldEqual` "CREATE TABLE f64docs (embedding F64_BLOB(3) NOT NULL, id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL)"

  describe "Type-level vector_distance_l2" do
    it "where_ with vector_distance_l2 multi-arg" \_ -> do
      toSQL typedWhereVectorDistL2 `shouldEqual` "SELECT * FROM vdocs WHERE vector_distance_l2(emb, $probe) < $maxDist"

    it "select with vector_distance_l2 alias" \_ -> do
      toSQL typedSelectVectorDistL2 `shouldEqual` "SELECT title, vector_distance_l2(emb, $probe) AS dist FROM vdocs WHERE vector_distance_l2(emb, $probe) IS NOT NULL"

    it "VECTOR_DISTANCE_L2 uppercase" \_ -> do
      toSQL typedWhereVectorDistL2Upper `shouldEqual` "SELECT * FROM vdocs WHERE VECTOR_DISTANCE_L2(emb, $probe) < $maxDist"

    it "vector_distance_l2 with custom alias" \_ -> do
      toSQL typedSelectDistL2Alias `shouldEqual` "SELECT title, vector_distance_l2(emb, $probe) AS l2dist FROM vdocs WHERE vector_distance_l2(emb, $probe) IS NOT NULL"

  describe "Compile-time golden tests" do
    it "F64Vector in selectAll result type" \_ -> do
      toSQL typedF64DocsSelectAll `shouldEqual` "SELECT * FROM f64docs"

    it "RandomRowId insert excludes id" \_ -> do
      toSQL typedRandomRowIdInsert `shouldEqual` "INSERT INTO things (name) VALUES (?1)"

  describe "Edge cases" do
    it "Maybe Just as SQLiteValue passes the inner value" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      SQLite.execute
        (SQLite.SQL "INSERT INTO users (name, email, age) VALUES (?1, ?2, ?3)")
        [ SQLite.toSQLiteValue "Alice"
        , SQLite.toSQLiteValue "a@b.com"
        , SQLite.toSQLiteValue (Just 42 :: Maybe Int)
        ]
        conn # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let ages = map (_.age) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      ages `shouldEqual` [Just 42]

    it "Boolean round-trip via ToSQLiteValue uses 0/1" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @ConfigTable)) conn # void
      SQLite.execute (SQLite.SQL "INSERT INTO config (active) VALUES (?1)") [ SQLite.toSQLiteValue true ] conn # void
      SQLite.execute (SQLite.SQL "INSERT INTO config (active) VALUES (?1)") [ SQLite.toSQLiteValue false ] conn # void
      rows <- runQuery conn {} (from (Proxy :: Proxy ConfigTable) # selectAll # orderBy @"id")
      let actives = map (_.active) (rows :: Array { id :: Int, active :: SQLiteBool, role :: String, score :: Int })
      actives `shouldEqual` [SQLiteBool true, SQLiteBool false]

    it "batch with single statement" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      results <- SQLite.batch SQLite.Write
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Solo", "s@t.com"] } ]
        conn
      Array.length results `shouldEqual` 1
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Solo"]

    it "batch with Deferred mode" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      results <- SQLite.batch SQLite.Deferred
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["A", "a@b.com"] }
        , { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["B", "b@c.com"] }
        ]
        conn
      Array.length results `shouldEqual` 2

    it "lastInsertRowid is populated as BigInt" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      result <- SQLite.query (SQLite.SQL "INSERT INTO users (name, email) VALUES (?1, ?2)") (map SQLite.toSQLiteValue ["Alice", "a@b.com"]) conn
      result.lastInsertRowid `shouldEqual` Just (JS.BigInt.fromInt 1)

    it "multiple inserts produce incrementing rowids" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      r1 <- SQLite.query (SQLite.SQL "INSERT INTO users (name, email) VALUES (?1, ?2)") (map SQLite.toSQLiteValue ["A", "a@b.com"]) conn
      r2 <- SQLite.query (SQLite.SQL "INSERT INTO users (name, email) VALUES (?1, ?2)") (map SQLite.toSQLiteValue ["B", "b@c.com"]) conn
      r1.lastInsertRowid `shouldEqual` Just (JS.BigInt.fromInt 1)
      r2.lastInsertRowid `shouldEqual` Just (JS.BigInt.fromInt 2)

  describe "Evil edge cases" do
    it "single quotes in string values don't corrupt SQL" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "O'Brien", email: "o@b.com" }) # void
      rows <- runQuery conn { name: "O'Brien" } (from usersTable # selectAll # where_ @"name = $name")
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["O'Brien"]

    it "empty string values round-trip" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "", email: "" }) # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` [""]

    it "same named param used twice in WHERE" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "Alice" }) # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "other" }) # void
      rows <- runQuery conn { val: "Alice" }
        (from usersTable # selectAll # whereRaw @"name = $val AND email = $val" @(val :: String))
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice"]

    it "rollback undoes batch in transaction" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      SQLite.txBatch
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["A", "a@b.com"] }
        , { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["B", "b@c.com"] }
        ]
        txn # void
      SQLite.rollback txn
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` []
      SQLite.close conn # liftEffect

    it "batch fails atomically on constraint violation" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      expectError $ SQLite.batch SQLite.Write
        [ { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Bob", "b@c.com"] }
        , { sql: "INSERT INTO users (name, email) VALUES (?1, ?2)", args: map SQLite.toSQLiteValue ["Dupe", "a@b.com"] }
        ]
        conn
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` ["Alice"]

    it "executeMultiple with trailing semicolons" \conn -> do
      SQLite.executeMultiple (createTableDDL @UsersTable <> ";;;") conn
      runExecute conn {} (from usersTable # insert { name: "ok", email: "ok@ok.com" }) # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      Array.length rows `shouldEqual` 1

    it "double commit throws" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      SQLite.commit txn
      expectError $ SQLite.commit txn
      SQLite.close conn # liftEffect

    it "commit then rollback throws" \_ -> do
      conn <- withTempDb
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      txn <- SQLite.begin conn
      SQLite.commit txn
      expectError $ SQLite.rollback txn
      SQLite.close conn # liftEffect

    it "unicode and emoji in string values" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      let val = "日本語テスト 🎉"
      runExecute conn {} (from usersTable # insert { name: val, email: "e@e.com" }) # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let names = map (_.name) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      names `shouldEqual` [val]

    it "all nullable columns set to NULL" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Ghost", email: "g@g.com", age: Nothing :: Maybe Int }) # void
      rows <- runQuery conn {} (from usersTable # selectAll)
      let ages = map (_.age) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      ages `shouldEqual` [Nothing]

    it "negative integers round-trip" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Neg", email: "n@n.com", age: Just (-42) }) # void
      rows <- runQuery conn { age: -42 } (from usersTable # selectAll # where_ @"age = $age")
      let ages = map (_.age) (rows :: Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
      ages `shouldEqual` [Just (-42)]

    it "insert with all-default columns uses DEFAULT VALUES" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @ConfigTable)) conn # void
      runExecute conn {} (from (Proxy :: Proxy ConfigTable) # insert {}) # void
      rows <- runQuery conn {} (from (Proxy :: Proxy ConfigTable) # selectAll)
      let scores = map (_.score) (rows :: Array { id :: Int, active :: SQLiteBool, role :: String, score :: Int })
      scores `shouldEqual` [0]

    it "insert overriding default values" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @ConfigTable)) conn # void
      runExecute conn {} (from (Proxy :: Proxy ConfigTable) # insert { score: 99, role: "admin" }) # void
      rows <- runQuery conn {} (from (Proxy :: Proxy ConfigTable) # selectAll)
      let row = rows :: Array { id :: Int, active :: SQLiteBool, role :: String, score :: Int }
      map (_.score) row `shouldEqual` [99]
      map (_.role) row `shouldEqual` ["admin"]

    it "query after connection close throws" \_ -> do
      conn <- withTempDb
      SQLite.close conn # liftEffect
      expectError $ SQLite.querySimple (SQLite.SQL "SELECT 1") conn

    it "concurrent reads on same connection" \conn -> do
      SQLite.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) conn # void
      runExecute conn {} (from usersTable # insert { name: "Alice", email: "a@b.com" }) # void
      results <- parSequence
        [ runQuery conn {} (from usersTable # selectAll)
        , runQuery conn {} (from usersTable # selectAll)
        ]
      let lengths = map Array.length (results :: Array (Array { id :: Int, name :: String, email :: String, age :: Maybe Int }))
      lengths `shouldEqual` [1, 1]

composeFile :: Docker.ComposeFile
composeFile = Docker.ComposeFile "docker-compose.test.yml"

libsqlUrl :: String
libsqlUrl = "http://127.0.0.1:8090"

withLibSql :: (SQLite.Connection -> Aff Unit) -> Aff Unit
withLibSql test = do
  conn <- SQLite.sqlite { url: libsqlUrl } # liftEffect
  test conn
  SQLite.close conn # liftEffect

libsqlSpec :: Spec Unit
libsqlSpec = around withLibSql do
  describe "Turso vector (against libsql server)" do
    it "vector insert and read round-trip" \conn -> do
      SQLite.executeSimple (SQLite.SQL "DROP TABLE IF EXISTS vdocs") conn # void
      SQLite.executeSimple (SQLite.SQL (createTableDDL @VDocsTable)) conn # void
      let v1 = f32Vector @"3" [1.0, 0.0, 0.0]
      let v2 = f32Vector @"3" [0.0, 1.0, 0.0]
      runExecute conn {} (from vDocsTable # insert { title: "doc1", emb: v1 }) # void
      runExecute conn {} (from vDocsTable # insert { title: "doc2", emb: v2 }) # void
      rows <- runQuery conn {} (from vDocsTable # selectAll # orderBy @"id")
      let titles = map (_.title) (rows :: Array { id :: Int, title :: String, emb :: F32Vector "3" })
      titles `shouldEqual` ["doc1", "doc2"]
      let vecs = map (\r -> unF32Vector r.emb) rows
      vecs `shouldEqual` [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]]

    it "vector_distance_cos" \conn -> do
      SQLite.executeSimple (SQLite.SQL "DROP TABLE IF EXISTS vdocs") conn # void
      SQLite.executeSimple (SQLite.SQL (createTableDDL @VDocsTable)) conn # void
      let v1 = f32Vector @"3" [1.0, 0.0, 0.0]
      let v2 = f32Vector @"3" [0.0, 1.0, 0.0]
      runExecute conn {} (from vDocsTable # insert { title: "doc1", emb: v1 }) # void
      runExecute conn {} (from vDocsTable # insert { title: "doc2", emb: v2 }) # void
      let probe = f32Vector @"3" [1.0, 0.0, 0.0]
      distRows <- runQuery conn { probe: probe }
        ( from vDocsTable
            # select @"title, vector_distance_cos(emb, $probe) AS dist"
            # where_ @"vector_distance_cos(emb, $probe) IS NOT NULL"
            # orderByRaw @"dist ASC"
        )
      let results = map (\r -> r.title /\ r.dist) (distRows :: Array { title :: String, dist :: Number })
      results `shouldEqual` [("doc1" /\ 0.0), ("doc2" /\ 1.0)]

    it "vector_top_k via fromRaw" \conn -> do
      SQLite.executeSimple (SQLite.SQL "DROP INDEX IF EXISTS topk_idx") conn # void
      SQLite.executeSimple (SQLite.SQL "DROP TABLE IF EXISTS topk_docs") conn # void
      SQLite.executeSimple (SQLite.SQL "CREATE TABLE topk_docs (id INTEGER PRIMARY KEY, title TEXT NOT NULL, emb F32_BLOB(3) NOT NULL)") conn # void
      SQLite.executeSimple (SQLite.SQL ("INSERT INTO topk_docs VALUES (1, 'north', " <> vector32 [1.0, 0.0, 0.0] <> ")")) conn # void
      SQLite.executeSimple (SQLite.SQL ("INSERT INTO topk_docs VALUES (2, 'east', " <> vector32 [0.0, 1.0, 0.0] <> ")")) conn # void
      SQLite.executeSimple (SQLite.SQL ("INSERT INTO topk_docs VALUES (3, 'northeast', " <> vector32 [0.7, 0.7, 0.0] <> ")")) conn # void
      SQLite.executeSimple (SQLite.SQL "CREATE INDEX topk_idx ON topk_docs (libsql_vector_idx(emb))") conn # void
      let probe = f32Vector @"3" [1.0, 0.0, 0.0]
      rows <- runQuery conn { probe: probe }
        ( fromRaw @"vector_top_k('topk_idx', $probe, 2) AS top JOIN topk_docs ON topk_docs.rowid = top.id"
            # selectRaw @"topk_docs.title" @(title :: String)
            # whereRaw @"1 = 1" @(probe :: F32Vector "3")
        )
      let titles = map (_.title) (rows :: Array { title :: String })
      titles `shouldEqual` ["north", "northeast"]

main :: Effect Unit
main = launchAff_ do
  bracket
    (Docker.startService composeFile (Docker.Timeout (30.0 # Seconds # fromDuration)))
    (\_ -> Docker.stopService composeFile)
    ( \_ -> runSpec [ consoleReporter ] do
        spec
        libsqlSpec
    )
