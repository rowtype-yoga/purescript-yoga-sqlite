module Yoga.SQLite.SQLite where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.JSDate as JSDate
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Prim.Row (class Union)
import Promise (Promise)
import Promise.Aff (toAffE) as Promise
import Unsafe.Coerce (unsafeCoerce)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Opaque Foreign Types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

foreign import data Connection :: Type
foreign import data Transaction :: Type
foreign import data SQLiteValue :: Type

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Newtypes for Type Safety
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype SQL = SQL String

derive instance Newtype SQL _
derive newtype instance Eq SQL
derive newtype instance Show SQL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-safe SQL parameters
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ToSQLiteValue a where
  toSQLiteValue :: a -> SQLiteValue

instance ToSQLiteValue String where
  toSQLiteValue = unsafeCoerce

instance ToSQLiteValue Int where
  toSQLiteValue = unsafeCoerce

instance ToSQLiteValue Number where
  toSQLiteValue = unsafeCoerce

instance ToSQLiteValue Foreign where
  toSQLiteValue = unsafeCoerce

param :: forall a. ToSQLiteValue a => a -> Array SQLiteValue
param x = [ toSQLiteValue x ]

params :: forall a. ToSQLiteValue a => Array a -> Array SQLiteValue
params xs = map toSQLiteValue xs

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Result types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type Row = Foreign

type QueryResult =
  { rows :: Array Row
  , columns :: Array String
  , columnTypes :: Array String
  , rowsAffected :: Int
  , lastInsertRowid :: Maybe Int
  }

type QueryResultImpl =
  { rows :: Array Row
  , columns :: Array String
  , columnTypes :: Array String
  , rowsAffected :: Int
  , lastInsertRowid :: Nullable Int
  }

fromQueryResultImpl :: QueryResultImpl -> QueryResult
fromQueryResultImpl r =
  { rows: r.rows
  , columns: r.columns
  , columnTypes: r.columnTypes
  , rowsAffected: r.rowsAffected
  , lastInsertRowid: Nullable.toMaybe r.lastInsertRowid
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Connection configuration
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type SQLiteConfigImpl =
  ( url :: String
  , authToken :: String
  , syncUrl :: String
  , syncInterval :: Number
  , encryptionKey :: String
  , tls :: Boolean
  , intMode :: String
  , concurrency :: Int
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- FFI Imports
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

foreign import createClientImpl :: forall opts. EffectFn1 { | opts } Connection
foreign import closeImpl :: EffectFn1 Connection (Promise Unit)

foreign import queryImpl :: EffectFn3 Connection String (Array SQLiteValue) (Promise QueryResultImpl)
foreign import queryOneImpl :: EffectFn3 Connection String (Array SQLiteValue) (Promise (Nullable Row))
foreign import executeImpl :: EffectFn3 Connection String (Array SQLiteValue) (Promise Int)
foreign import querySimpleImpl :: EffectFn2 Connection String (Promise QueryResultImpl)
foreign import executeSimpleImpl :: EffectFn2 Connection String (Promise Int)

foreign import beginImpl :: EffectFn1 Connection (Promise Transaction)
foreign import beginWithModeImpl :: EffectFn2 Connection String (Promise Transaction)
foreign import commitImpl :: EffectFn1 Transaction (Promise Unit)
foreign import rollbackImpl :: EffectFn1 Transaction (Promise Unit)

foreign import txQueryImpl :: EffectFn3 Transaction String (Array SQLiteValue) (Promise QueryResultImpl)
foreign import txQueryOneImpl :: EffectFn3 Transaction String (Array SQLiteValue) (Promise (Nullable Row))
foreign import txExecuteImpl :: EffectFn3 Transaction String (Array SQLiteValue) (Promise Int)

foreign import txCloseImpl :: EffectFn1 Transaction Unit
foreign import txBatchImpl :: EffectFn2 Transaction (Array BatchStatement) (Promise (Array QueryResultImpl))
foreign import txExecuteMultipleImpl :: EffectFn2 Transaction String (Promise Unit)

type BatchStatement = { sql :: String, args :: Array SQLiteValue }

foreign import batchImpl :: EffectFn3 Connection (Array BatchStatement) String (Promise (Array QueryResultImpl))

type ReplicatedImpl = { frame_no :: Int, frames_synced :: Int }
type Replicated = { frame_no :: Int, frames_synced :: Int }

foreign import syncImpl :: EffectFn1 Connection (Promise (Nullable ReplicatedImpl))
foreign import executeMultipleImpl :: EffectFn2 Connection String (Promise Unit)
foreign import migrateImpl :: EffectFn2 Connection (Array BatchStatement) (Promise (Array QueryResultImpl))

foreign import pingImpl :: EffectFn1 Connection (Promise Boolean)

foreign import dateTimeToStringImpl :: JSDate.JSDate -> String

foreign import f32VectorFromArrayImpl :: Array Number -> Foreign

f32VectorFromArray :: Array Number -> Foreign
f32VectorFromArray = f32VectorFromArrayImpl

foreign import f32VectorToArrayImpl :: Foreign -> Array Number

f32VectorToArray :: Foreign -> Array Number
f32VectorToArray = f32VectorToArrayImpl

foreign import f64VectorFromArrayImpl :: Array Number -> Foreign

f64VectorFromArray :: Array Number -> Foreign
f64VectorFromArray = f64VectorFromArrayImpl

foreign import f64VectorToArrayImpl :: Foreign -> Array Number

f64VectorToArray :: Foreign -> Array Number
f64VectorToArray = f64VectorToArrayImpl

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Connection Management
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

sqlite :: forall opts opts_. Union opts opts_ SQLiteConfigImpl => { | opts } -> Effect Connection
sqlite opts = runEffectFn1 createClientImpl opts

close :: Connection -> Aff Unit
close = runEffectFn1 closeImpl >>> Promise.toAffE

ping :: Connection -> Aff Boolean
ping = runEffectFn1 pingImpl >>> Promise.toAffE

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query Operations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

query :: SQL -> Array SQLiteValue -> Connection -> Aff QueryResult
query (SQL sql) args conn =
  runEffectFn3 queryImpl conn sql args # Promise.toAffE <#> fromQueryResultImpl

querySimple :: SQL -> Connection -> Aff QueryResult
querySimple (SQL sql) conn =
  runEffectFn2 querySimpleImpl conn sql # Promise.toAffE <#> fromQueryResultImpl

queryOne :: SQL -> Array SQLiteValue -> Connection -> Aff (Maybe Row)
queryOne (SQL sql) args conn =
  runEffectFn3 queryOneImpl conn sql args # Promise.toAffE <#> Nullable.toMaybe

execute :: SQL -> Array SQLiteValue -> Connection -> Aff Int
execute (SQL sql) args conn =
  runEffectFn3 executeImpl conn sql args # Promise.toAffE

executeSimple :: SQL -> Connection -> Aff Int
executeSimple (SQL sql) conn =
  runEffectFn2 executeSimpleImpl conn sql # Promise.toAffE

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Batch Operations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

batch :: TransactionMode -> Array BatchStatement -> Connection -> Aff (Array QueryResult)
batch mode stmts conn =
  runEffectFn3 batchImpl conn stmts (transactionModeToString mode) # Promise.toAffE <#> map fromQueryResultImpl

sync :: Connection -> Aff (Maybe Replicated)
sync = runEffectFn1 syncImpl >>> Promise.toAffE >>> map Nullable.toMaybe

executeMultiple :: String -> Connection -> Aff Unit
executeMultiple sql conn =
  runEffectFn2 executeMultipleImpl conn sql # Promise.toAffE

migrate :: Array BatchStatement -> Connection -> Aff (Array QueryResult)
migrate stmts conn =
  runEffectFn2 migrateImpl conn stmts # Promise.toAffE <#> map fromQueryResultImpl

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Transaction Operations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data TransactionMode = Write | Read | Deferred

transactionModeToString :: TransactionMode -> String
transactionModeToString = case _ of
  Write -> "write"
  Read -> "read"
  Deferred -> "deferred"

begin :: Connection -> Aff Transaction
begin = runEffectFn1 beginImpl >>> Promise.toAffE

beginWithMode :: TransactionMode -> Connection -> Aff Transaction
beginWithMode mode conn =
  runEffectFn2 beginWithModeImpl conn (transactionModeToString mode) # Promise.toAffE

commit :: Transaction -> Aff Unit
commit = runEffectFn1 commitImpl >>> Promise.toAffE

rollback :: Transaction -> Aff Unit
rollback = runEffectFn1 rollbackImpl >>> Promise.toAffE

txQuery :: SQL -> Array SQLiteValue -> Transaction -> Aff QueryResult
txQuery (SQL sql) args txn =
  runEffectFn3 txQueryImpl txn sql args # Promise.toAffE <#> fromQueryResultImpl

txQueryOne :: SQL -> Array SQLiteValue -> Transaction -> Aff (Maybe Row)
txQueryOne (SQL sql) args txn =
  runEffectFn3 txQueryOneImpl txn sql args # Promise.toAffE <#> Nullable.toMaybe

txExecute :: SQL -> Array SQLiteValue -> Transaction -> Aff Int
txExecute (SQL sql) args txn =
  runEffectFn3 txExecuteImpl txn sql args # Promise.toAffE

txClose :: Transaction -> Effect Unit
txClose = runEffectFn1 txCloseImpl

txBatch :: Array BatchStatement -> Transaction -> Aff (Array QueryResult)
txBatch stmts txn =
  runEffectFn2 txBatchImpl txn stmts # Promise.toAffE <#> map fromQueryResultImpl

txExecuteMultiple :: String -> Transaction -> Aff Unit
txExecuteMultiple sql txn =
  runEffectFn2 txExecuteMultipleImpl txn sql # Promise.toAffE

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DateTime conversion helper
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

dateTimeToString :: JSDate.JSDate -> String
dateTimeToString = dateTimeToStringImpl
