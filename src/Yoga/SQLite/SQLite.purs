module Yoga.SQLite.SQLite where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Promise (Promise)
import Promise.Aff (toAffE) as Promise
import Yoga.SQL.Types (class FromResultArray, SQLParameter, SQLResult, fromResultArray)
import Unsafe.Coerce (unsafeCoerce)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Opaque Foreign Types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

foreign import data DBConnection :: Type
foreign import data Transaction :: Type
foreign import data Statement :: Type -> Type -> Type

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Newtypes for Type Safety
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype DatabasePath = DatabasePath String

derive instance Newtype DatabasePath _
derive newtype instance Show DatabasePath

newtype NumberOfChanges = NumberOfChanges Int

derive instance Newtype NumberOfChanges _
derive newtype instance Show NumberOfChanges

newtype RowId = RowId Int

derive instance Newtype RowId _
derive newtype instance Show RowId

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- FFI Imports
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

foreign import openImpl :: EffectFn1 String DBConnection
foreign import closeImpl :: EffectFn1 DBConnection Unit
foreign import execImpl :: EffectFn3 String (Array SQLParameter) DBConnection Unit
foreign import queryImpl :: EffectFn3 String (Array SQLParameter) DBConnection (Array (Array SQLResult))
foreign import queryOneImpl :: EffectFn3 String (Array SQLParameter) DBConnection (Nullable (Array SQLResult))
foreign import lastInsertRowIdImpl :: EffectFn1 DBConnection Int
foreign import beginTransactionImpl :: EffectFn1 DBConnection Transaction
foreign import commitImpl :: EffectFn1 Transaction Unit
foreign import rollbackImpl :: EffectFn1 Transaction Unit

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Database Operations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Open a database connection
open :: DatabasePath -> Effect DBConnection
open (DatabasePath path) = runEffectFn1 openImpl path

-- | Close a database connection
close :: DBConnection -> Effect Unit
close = runEffectFn1 closeImpl

-- | Execute a statement (INSERT/UPDATE/DELETE)
exec :: String -> Array SQLParameter -> DBConnection -> Effect Unit
exec sql params db = runEffectFn3 execImpl sql params db

-- | Query for multiple rows
query :: String -> Array SQLParameter -> DBConnection -> Effect (Array (Array SQLResult))
query sql params db = runEffectFn3 queryImpl sql params db

-- | Query for a single row
queryOne :: String -> Array SQLParameter -> DBConnection -> Effect (Maybe (Array SQLResult))
queryOne sql params db = runEffectFn3 queryOneImpl sql params db <#> Nullable.toMaybe

-- | Get last inserted row ID
lastInsertRowId :: DBConnection -> Effect Int
lastInsertRowId = runEffectFn1 lastInsertRowIdImpl

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Transaction Support
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Begin a transaction
beginTransaction :: DBConnection -> Effect Transaction
beginTransaction = runEffectFn1 beginTransactionImpl

-- | Commit a transaction
commit :: Transaction -> Effect Unit
commit = runEffectFn1 commitImpl

-- | Rollback a transaction
rollback :: Transaction -> Effect Unit
rollback = runEffectFn1 rollbackImpl

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Prepared Statements
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype PreparedStatement :: forall k1 k2. k1 -> k2 -> Type
newtype PreparedStatement i o = PreparedStatement
  { sql :: String
  , db :: DBConnection
  }

-- | Prepare a statement
prepare :: forall @i @o. String -> DBConnection -> Effect (Statement i o)
prepare sql db = pure $ unsafeCoerce (PreparedStatement { sql, db })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Info Type for Mutations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type InfoImpl = { changes :: Int, lastInsertRowid :: Nullable Int }

type Info = { changes :: NumberOfChanges, lastInsertRowid :: Maybe RowId }

fromRowInfoImpl :: InfoImpl -> Info
fromRowInfoImpl = \{ changes, lastInsertRowid } ->
  { changes: NumberOfChanges changes
  , lastInsertRowid: Nullable.toMaybe lastInsertRowid <#> RowId
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Statement Execution
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Run a statement (INSERT/UPDATE/DELETE)
run :: forall i. Array SQLParameter -> Statement i Void -> Effect Info
run params st = do
  let PreparedStatement { sql, db } = unsafeCoerce st
  exec sql params db
  -- Get last insert row ID
  rowId <- lastInsertRowId db
  pure { changes: NumberOfChanges 1, lastInsertRowid: Just (RowId rowId) }

-- | Run a statement with no parameters
run_ :: Statement Void Void -> Effect Info
run_ = run []

-- | Query all rows (raw results)
allRaw :: forall i o. Array SQLParameter -> Statement i o -> Effect (Array (Array SQLResult))
allRaw params st = do
  let PreparedStatement { sql, db } = unsafeCoerce st
  query sql params db

-- | Query all rows with result parsing
all ::
  forall i @o.
  Array SQLParameter ->
  (Array SQLResult -> Either String o) ->
  Statement i o ->
  Effect (Either String (Array o))
all params toOutput st = do
  rows <- allRaw params st
  pure $ traverse toOutput rows

-- | Query exactly one row
all1 ::
  forall i o.
  Array SQLParameter ->
  (Array SQLResult -> Either String o) ->
  Statement i o ->
  Effect (Either String o)
all1 params fn st =
  all params fn st
    <#> case _ of
      Right [ x ] -> Right x
      Right [] -> Left "Expected a single result, got empty array"
      Right arr -> Left $ "Expected a single result, got " <> show (length arr)
      Left err -> Left err

-- | Query all rows with no parameters
all_ :: forall o. Array SQLParameter -> Statement Void o -> Effect (Array (Array SQLResult))
all_ params = allRaw params

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Statement Metadata
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type StatementSource = String

statementSource :: forall i o. Statement i o -> StatementSource
statementSource = unsafeCoerce >>> \(PreparedStatement rec) -> rec.sql
