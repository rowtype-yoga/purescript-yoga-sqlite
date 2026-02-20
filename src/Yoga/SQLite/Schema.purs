module Yoga.SQLite.Schema where

import Prelude

import Data.Array as Array
import Data.Array (intercalate, mapWithIndex, foldl)
import Data.Date (Date, exactDate)
import Data.Enum (toEnum, fromEnum)
import Data.DateTime (DateTime(..), date, time)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time (Time(..), hour, minute, second)
import Data.Newtype (class Newtype, un)
import Data.Nullable (toNullable)
import Data.UUID (UUID)
import JS.BigInt (BigInt)
import Unsafe.Coerce (unsafeCoerce)
import Data.Reflectable (class Reflectable, reflectType)
import Data.String.Regex (regex, replace') as Regex
import Data.String.Regex (Regex) as Regex
import Data.String.Regex.Flags (global) as Regex
import Control.Monad.Except (except, runExcept)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List.Types (NonEmptyList)
import Data.Number as Number
import Data.String.Common as Str
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Effect.Exception (error) as Exception
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Function (type (#))
import Effect.Aff (Aff, throwError)
import Foreign (Foreign, ForeignError(..), unsafeToForeign)
import Prim.Boolean (True, False)
import Prim.Row (class Cons, class Lacks, class Union, class Nub) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.Symbol (class Cons, class Append) as Symbol
import Prim.TypeError (class Fail, Beside, Text, Quote)
import Record (get) as Record
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Yoga.JSON (class ReadForeign, readImpl, unsafeStringify)
import Yoga.SQLite.SQLite as SQLite

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Core phantom types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Table :: Symbol -> Row Type -> Type
data Table name columns = Table

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraint wrappers (used with # operator from Type.Function)
--   Int # PrimaryKey # AutoIncrement = AutoIncrement (PrimaryKey Int)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data PrimaryKey :: Type -> Type
data PrimaryKey a

data AutoIncrement :: Type -> Type
data AutoIncrement a

data Unique :: Type -> Type
data Unique a

data Default :: forall k. k -> Type -> Type
data Default val a

data References

data ForeignKey :: Symbol -> Type -> Symbol -> Type -> Type
data ForeignKey table references col a

data DefaultExpr :: Symbol -> Type -> Type
data DefaultExpr expr a

data RandomRowId :: Type -> Type
data RandomRowId a

-- Internal: used by LEFT JOIN to mark columns as nullable
data Nullable :: Type -> Type
data Nullable a

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JSON type wrapper (stored as TEXT in SQLite)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype Json = Json Foreign

derive instance Newtype Json _

instance ReadForeign Json where
  readImpl = pure <<< Json

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SQLUUID: newtype over Data.UUID.UUID (stored as TEXT in SQLite)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype SQLUUID = SQLUUID UUID

derive instance Newtype SQLUUID _
derive newtype instance Eq SQLUUID
derive newtype instance Ord SQLUUID
derive newtype instance Show SQLUUID

instance ReadForeign SQLUUID where
  readImpl f = do
    s :: String <- readImpl f
    pure (unsafeCoerce s)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SQLDate / SQLTime: newtypes for DATE and TIME columns
-- (stored as TEXT in SQLite: "YYYY-MM-DD" and "HH:MM:SS")
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype SQLDate = SQLDate Date

derive instance Newtype SQLDate _
derive instance Eq SQLDate
derive instance Ord SQLDate
instance Show SQLDate where
  show (SQLDate d) = "(SQLDate " <> show d <> ")"

instance ReadForeign SQLDate where
  readImpl f = do
    s :: String <- readImpl f
    except $ note (pure (ForeignError ("Invalid date: " <> s))) (parseDate s)
    where
    parseDate str = do
      let parts = Str.split (Pattern "-") str
      case parts of
        [ yStr, mStr, dStr ] -> do
          y <- Int.fromString yStr >>= toEnum
          m <- Int.fromString mStr >>= toEnum
          d <- Int.fromString dStr >>= toEnum
          exactDate y m d <#> SQLDate
        _ -> Nothing

newtype SQLTime = SQLTime Time

derive instance Newtype SQLTime _
derive instance Eq SQLTime
derive instance Ord SQLTime
instance Show SQLTime where
  show (SQLTime t) = "(SQLTime " <> show t <> ")"

instance ReadForeign SQLTime where
  readImpl f = do
    s :: String <- readImpl f
    except $ note (pure (ForeignError ("Invalid time: " <> s))) (parseTime s)
    where
    parseTime str = do
      let parts = Str.split (Pattern ":") str
      case parts of
        [ hStr, mStr, sStr ] -> do
          h <- Int.fromString hStr >>= toEnum
          mi <- Int.fromString mStr >>= toEnum
          let secParts = Str.split (Pattern ".") sStr
          sec <- Int.fromString (fromMaybe sStr (Array.head secParts)) >>= toEnum
          let ms = fromMaybe bottom (Array.index secParts 1 >>= Int.fromString >>= toEnum)
          pure (SQLTime (Time h mi sec ms))
        _ -> Nothing

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SQLiteBool: boolean stored as INTEGER 0/1 in SQLite
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype SQLiteBool = SQLiteBool Boolean

derive instance Newtype SQLiteBool _
derive newtype instance Eq SQLiteBool
derive newtype instance Ord SQLiteBool
derive newtype instance Show SQLiteBool

instance ReadForeign SQLiteBool where
  readImpl f = do
    n :: Int <- readImpl f
    pure (SQLiteBool (n /= 0))

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- F32Vector: Turso F32_BLOB vector column (dim is a Symbol like "3", "768")
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype F32Vector :: Symbol -> Type
newtype F32Vector dim = F32Vector Foreign

derive instance Newtype (F32Vector dim) _

f32Vector :: forall @dim. Array Number -> F32Vector dim
f32Vector arr = F32Vector (SQLite.f32VectorFromArray arr)

unF32Vector :: forall dim. F32Vector dim -> Array Number
unF32Vector (F32Vector buf) = SQLite.f32VectorToArray buf

vector32 :: Array Number -> String
vector32 nums = "vector32('[" <> intercalate ", " (map show nums) <> "]')"

instance ReadForeign (F32Vector dim) where
  readImpl = pure <<< F32Vector

instance SQLite.ToSQLiteValue (F32Vector dim) where
  toSQLiteValue (F32Vector f) = unsafeCoerce f

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- F64Vector: Turso F64_BLOB vector column (dim is a Symbol like "3", "768")
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype F64Vector :: Symbol -> Type
newtype F64Vector dim = F64Vector Foreign

derive instance Newtype (F64Vector dim) _

f64Vector :: forall @dim. Array Number -> F64Vector dim
f64Vector arr = F64Vector (SQLite.f64VectorFromArray arr)

unF64Vector :: forall dim. F64Vector dim -> Array Number
unF64Vector (F64Vector buf) = SQLite.f64VectorToArray buf

instance ReadForeign (F64Vector dim) where
  readImpl = pure <<< F64Vector

instance SQLite.ToSQLiteValue (F64Vector dim) where
  toSQLiteValue (F64Vector f) = unsafeCoerce f

instance SQLite.ToSQLiteValue SQLiteBool where
  toSQLiteValue (SQLiteBool b) = unsafeCoerce (if b then 1 else 0)

instance SQLite.ToSQLiteValue SQLDate where
  toSQLiteValue (SQLDate d) = unsafeCoerce (SCU.take 10 (SQLite.dateTimeToString (JSDate.fromDateTime (DateTime d bottom))))

instance SQLite.ToSQLiteValue SQLTime where
  toSQLiteValue (SQLTime t) = unsafeCoerce (pad (fromEnum (hour t)) <> ":" <> pad (fromEnum (minute t)) <> ":" <> pad (fromEnum (second t)))
    where
    pad n = if n < 10 then "0" <> show n else show n

instance SQLite.ToSQLiteValue SQLUUID where
  toSQLiteValue (SQLUUID uuid) = unsafeCoerce uuid

instance SQLite.ToSQLiteValue Json where
  toSQLiteValue (Json f) = unsafeCoerce (unsafeStringify f)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Nullability: inferred from Maybe
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsNullable a where
  isNullable :: Proxy a -> Boolean

instance IsNullable (Maybe a) where
  isNullable _ = true
else instance IsNullable a where
  isNullable _ = false

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ExtractType: recursively unwrap constraint wrappers to get base type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ExtractType :: Type -> Type -> Constraint
class ExtractType wrapped typ | wrapped -> typ

instance ExtractType a typ => ExtractType (PrimaryKey a) typ
else instance ExtractType a typ => ExtractType (AutoIncrement a) typ
else instance ExtractType a typ => ExtractType (Unique a) typ
else instance ExtractType a typ => ExtractType (Default val a) typ
else instance ExtractType a typ => ExtractType (DefaultExpr expr a) typ
else instance ExtractType a typ => ExtractType (ForeignKey t r c a) typ
else instance ExtractType a typ => ExtractType (RandomRowId a) typ
else instance ExtractType a typ => ExtractType (Nullable a) (Maybe typ)
else instance ExtractType a a

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- PureScript type -> SQLite type name
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SQLiteTypeName a where
  sqliteTypeName :: Proxy a -> String

instance SQLiteTypeName Int where
  sqliteTypeName _ = "INTEGER"

instance SQLiteTypeName String where
  sqliteTypeName _ = "TEXT"

instance SQLiteTypeName Boolean where
  sqliteTypeName _ = "INTEGER"

instance SQLiteTypeName SQLiteBool where
  sqliteTypeName _ = "INTEGER"

instance SQLiteTypeName Number where
  sqliteTypeName _ = "REAL"

instance SQLiteTypeName DateTime where
  sqliteTypeName _ = "TEXT"

instance SQLiteTypeName BigInt where
  sqliteTypeName _ = "INTEGER"

instance SQLiteTypeName SQLUUID where
  sqliteTypeName _ = "TEXT"

instance SQLiteTypeName SQLDate where
  sqliteTypeName _ = "TEXT"

instance SQLiteTypeName SQLTime where
  sqliteTypeName _ = "TEXT"

instance SQLiteTypeName Json where
  sqliteTypeName _ = "TEXT"

instance IsSymbol dim => SQLiteTypeName (F32Vector dim) where
  sqliteTypeName _ = "F32_BLOB(" <> reflectSymbol (Proxy :: Proxy dim) <> ")"

instance IsSymbol dim => SQLiteTypeName (F64Vector dim) where
  sqliteTypeName _ = "F64_BLOB(" <> reflectSymbol (Proxy :: Proxy dim) <> ")"

instance SQLiteTypeName a => SQLiteTypeName (Maybe a) where
  sqliteTypeName _ = sqliteTypeName (Proxy :: Proxy a)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraint -> DDL fragment
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class RenderDefaultValue val typ where
  renderDefaultValue :: Proxy val -> Proxy typ -> String

instance Reflectable sym String => RenderDefaultValue sym String where
  renderDefaultValue _ _ = "DEFAULT '" <> reflectType (Proxy :: Proxy sym) <> "'"

instance Reflectable val Int => RenderDefaultValue val Int where
  renderDefaultValue _ _ = "DEFAULT " <> show (reflectType (Proxy :: Proxy val))

instance Reflectable val Boolean => RenderDefaultValue val Boolean where
  renderDefaultValue _ _ = if reflectType (Proxy :: Proxy val) then "DEFAULT 1" else "DEFAULT 0"

instance Reflectable val Boolean => RenderDefaultValue val SQLiteBool where
  renderDefaultValue _ _ = if reflectType (Proxy :: Proxy val) then "DEFAULT 1" else "DEFAULT 0"

class RenderConstraint a where
  renderConstraint :: Proxy a -> String

instance RenderConstraint a => RenderConstraint (PrimaryKey a) where
  renderConstraint _ = joinConstraints "PRIMARY KEY" (renderConstraint (Proxy :: Proxy a))

else instance RenderConstraint a => RenderConstraint (AutoIncrement a) where
  renderConstraint _ = joinConstraints (renderConstraint (Proxy :: Proxy a)) "AUTOINCREMENT"

else instance RenderConstraint a => RenderConstraint (Unique a) where
  renderConstraint _ = joinConstraints "UNIQUE" (renderConstraint (Proxy :: Proxy a))

else instance (RenderDefaultValue val a, RenderConstraint a) => RenderConstraint (Default val a) where
  renderConstraint _ = joinConstraints (renderDefaultValue (Proxy :: Proxy val) (Proxy :: Proxy a)) (renderConstraint (Proxy :: Proxy a))

else instance (IsSymbol expr, RenderConstraint a) => RenderConstraint (DefaultExpr expr a) where
  renderConstraint _ = joinConstraints
    ("DEFAULT (" <> reflectSymbol (Proxy :: Proxy expr) <> ")")
    (renderConstraint (Proxy :: Proxy a))

else instance (IsSymbol table, IsSymbol col, RenderConstraint a) => RenderConstraint (ForeignKey table References col a) where
  renderConstraint _ = joinConstraints
    ("REFERENCES " <> reflectSymbol (Proxy :: Proxy table) <> "(" <> reflectSymbol (Proxy :: Proxy col) <> ")")
    (renderConstraint (Proxy :: Proxy a))

else instance RenderConstraint a => RenderConstraint (RandomRowId a) where
  renderConstraint _ = renderConstraint (Proxy :: Proxy a)

else instance RenderConstraint a => RenderConstraint (Nullable a) where
  renderConstraint _ = renderConstraint (Proxy :: Proxy a)

else instance RenderConstraint a where
  renderConstraint _ = ""

joinConstraints :: String -> String -> String
joinConstraints a b = case a, b of
  "", s -> s
  s, "" -> s
  s1, s2 -> s1 <> " " <> s2

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- HasAutoIncrementCheck: detect AUTOINCREMENT through wrappers
-- SQLite requires INTEGER PRIMARY KEY AUTOINCREMENT without NOT NULL
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class HasAutoIncrementCheck a where
  hasAutoIncrement :: Proxy a -> Boolean

instance HasAutoIncrementCheck (AutoIncrement a) where
  hasAutoIncrement _ = true
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (PrimaryKey a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (Unique a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (Default v a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (DefaultExpr e a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (ForeignKey t r c a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (RandomRowId a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a => HasAutoIncrementCheck (Nullable a) where
  hasAutoIncrement _ = hasAutoIncrement (Proxy :: Proxy a)
else instance HasAutoIncrementCheck a where
  hasAutoIncrement _ = false

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- HasRandomRowId: detect RandomRowId through wrappers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class HasRandomRowIdCheck a where
  hasRandomRowId :: Proxy a -> Boolean

instance HasRandomRowIdCheck (RandomRowId a) where
  hasRandomRowId _ = true
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (PrimaryKey a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (AutoIncrement a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (Unique a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (Default v a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (DefaultExpr e a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (ForeignKey t r c a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a => HasRandomRowIdCheck (Nullable a) where
  hasRandomRowId _ = hasRandomRowId (Proxy :: Proxy a)
else instance HasRandomRowIdCheck a where
  hasRandomRowId _ = false

class HasRandomRowIdRL :: RL.RowList Type -> Constraint
class HasRandomRowIdRL rl where
  hasRandomRowIdRL :: Proxy rl -> Boolean

instance HasRandomRowIdRL RL.Nil where
  hasRandomRowIdRL _ = false

instance (HasRandomRowIdCheck entry, HasRandomRowIdRL tail) => HasRandomRowIdRL (RL.Cons name entry tail) where
  hasRandomRowIdRL _ = hasRandomRowId (Proxy :: Proxy entry) || hasRandomRowIdRL (Proxy :: Proxy tail)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DDL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class CreateTableDDL a where
  createTableDDL :: String

class RenderColumnsRL :: RL.RowList Type -> Constraint
class RenderColumnsRL rl where
  renderColumnsRL :: Proxy rl -> Array String

instance RenderColumnsRL RL.Nil where
  renderColumnsRL _ = []

instance
  ( IsSymbol name
  , ExtractType entry typ
  , SQLiteTypeName typ
  , IsNullable typ
  , HasAutoIncrementCheck entry
  , RenderConstraint entry
  , RenderColumnsRL tail
  ) =>
  RenderColumnsRL (RL.Cons name entry tail) where
  renderColumnsRL _ = do
    let colName = reflectSymbol (Proxy :: Proxy name)
    let colType = sqliteTypeName (Proxy :: Proxy typ)
    let nullable = isNullable (Proxy :: Proxy typ)
    let isAutoInc = hasAutoIncrement (Proxy :: Proxy entry)
    let notNull = if nullable || isAutoInc then "" else " NOT NULL"
    let constraints = renderConstraint (Proxy :: Proxy entry)
    let constraintsSuffix = if constraints == "" then "" else " " <> constraints
    [ colName <> " " <> colType <> notNull <> constraintsSuffix ] <> renderColumnsRL (Proxy :: Proxy tail)

instance
  ( IsSymbol name
  , RowToList cols rl
  , RenderColumnsRL rl
  , HasRandomRowIdRL rl
  ) =>
  CreateTableDDL (Table name cols) where
  createTableDDL = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let columns = renderColumnsRL (Proxy :: Proxy rl)
    let suffix = if hasRandomRowIdRL (Proxy :: Proxy rl) then " RANDOM ROWID" else ""
    "CREATE TABLE " <> tableName <> " (" <> intercalate ", " columns <> ")" <> suffix

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Turso DDL helpers: vector and FTS index creation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

createVectorIndex
  :: forall @indexName @tableName @column @options
   . IsSymbol indexName
  => IsSymbol tableName
  => IsSymbol column
  => IsSymbol options
  => String
createVectorIndex = "CREATE INDEX " <> idx <> " ON " <> tbl <> " (libsql_vector_idx(" <> col <> ", '" <> opts <> "'))"
  where
  idx = reflectSymbol (Proxy :: Proxy indexName)
  tbl = reflectSymbol (Proxy :: Proxy tableName)
  col = reflectSymbol (Proxy :: Proxy column)
  opts = reflectSymbol (Proxy :: Proxy options)

createFTSIndex
  :: forall @indexName @tableName @columns
   . IsSymbol indexName
  => IsSymbol tableName
  => IsSymbol columns
  => String
createFTSIndex = "CREATE INDEX " <> idx <> " ON " <> tbl <> " USING fts (" <> cols <> ")"
  where
  idx = reflectSymbol (Proxy :: Proxy indexName)
  tbl = reflectSymbol (Proxy :: Proxy tableName)
  cols = reflectSymbol (Proxy :: Proxy columns)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- INSERT SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsAutoGenerated a where
  isAutoGenerated :: Proxy a -> Boolean

instance IsAutoGenerated (AutoIncrement a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated a => IsAutoGenerated (Default v a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (DefaultExpr expr a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (PrimaryKey a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (Unique a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (ForeignKey t r c a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated (RandomRowId a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated a => IsAutoGenerated (Nullable a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a where
  isAutoGenerated _ = false

class InsertColumnsRL :: RL.RowList Type -> Constraint
class InsertColumnsRL rl where
  insertColumnsRL :: Proxy rl -> Array String

instance InsertColumnsRL RL.Nil where
  insertColumnsRL _ = []

instance
  ( IsSymbol name
  , IsAutoGenerated entry
  , InsertColumnsRL tail
  ) =>
  InsertColumnsRL (RL.Cons name entry tail) where
  insertColumnsRL _ =
    let
      rest = insertColumnsRL (Proxy :: Proxy tail)
    in
      if isAutoGenerated (Proxy :: Proxy entry) then rest
      else [ reflectSymbol (Proxy :: Proxy name) ] <> rest

class InsertSQLFor a where
  insertSQLFor :: String

instance
  ( IsSymbol name
  , RowToList cols rl
  , InsertColumnsRL rl
  ) =>
  InsertSQLFor (Table name cols) where
  insertSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let cols = insertColumnsRL (Proxy :: Proxy rl)
    let placeholders = cols # mapWithIndex \i _ -> "?" <> show (i + 1)
    if Array.null cols then "INSERT INTO " <> tableName <> " DEFAULT VALUES RETURNING *"
    else "INSERT INTO " <> tableName
      <> " ("
      <> intercalate ", " cols
      <> ")"
      <> " VALUES ("
      <> intercalate ", " placeholders
      <> ")"
      <> " RETURNING *"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SELECT SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SelectAllSQLFor a where
  selectAllSQLFor :: String

instance
  ( IsSymbol name
  ) =>
  SelectAllSQLFor (Table name cols) where
  selectAllSQLFor = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)

class WhereClauseRL :: RL.RowList Type -> Constraint
class WhereClauseRL rl where
  whereClauseRL :: Proxy rl -> Int -> Array String

instance WhereClauseRL RL.Nil where
  whereClauseRL _ _ = []

instance (IsSymbol name, WhereClauseRL tail) => WhereClauseRL (RL.Cons name typ tail) where
  whereClauseRL _ idx =
    [ reflectSymbol (Proxy :: Proxy name) <> " = ?" <> show idx ]
      <> whereClauseRL (Proxy :: Proxy tail) (idx + 1)

class SelectWhereSQLFor a whereRow where
  selectWhereSQLFor :: String

instance
  ( IsSymbol name
  , RowToList whereRow whereRL
  , WhereClauseRL whereRL
  ) =>
  SelectWhereSQLFor (Table name cols) whereRow where
  selectWhereSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let conditions = whereClauseRL (Proxy :: Proxy whereRL) 1
    "SELECT * FROM " <> tableName <> " WHERE " <> intercalate " AND " conditions

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UPDATE SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ColumnCountRL :: RL.RowList Type -> Constraint
class ColumnCountRL rl where
  columnCountRL :: Proxy rl -> Int

instance ColumnCountRL RL.Nil where
  columnCountRL _ = 0

instance ColumnCountRL tail => ColumnCountRL (RL.Cons name typ tail) where
  columnCountRL _ = 1 + columnCountRL (Proxy :: Proxy tail)

class UpdateSQLFor table setRow whereRow where
  updateSQLFor :: String

instance
  ( IsSymbol name
  , RowToList setRow setRL
  , RowToList whereRow whereRL
  , WhereClauseRL setRL
  , WhereClauseRL whereRL
  , ColumnCountRL setRL
  ) =>
  UpdateSQLFor (Table name cols) setRow whereRow where
  updateSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let setClauses = whereClauseRL (Proxy :: Proxy setRL) 1
    let setCount = columnCountRL (Proxy :: Proxy setRL)
    let whereClauses = whereClauseRL (Proxy :: Proxy whereRL) (setCount + 1)
    "UPDATE " <> tableName
      <> " SET "
      <> intercalate ", " setClauses
      <> " WHERE "
      <> intercalate " AND " whereClauses

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DELETE SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class DeleteSQLFor table whereRow where
  deleteSQLFor :: String

instance
  ( IsSymbol name
  , RowToList whereRow whereRL
  , WhereClauseRL whereRL
  ) =>
  DeleteSQLFor (Table name cols) whereRow where
  deleteSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let conditions = whereClauseRL (Proxy :: Proxy whereRL) 1
    "DELETE FROM " <> tableName <> " WHERE " <> intercalate " AND " conditions

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Utility type classes
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class UnwrapMaybe :: Type -> Type -> Constraint
class UnwrapMaybe a b | a -> b

instance UnwrapMaybe (Maybe a) a
else instance UnwrapMaybe a a

-- Skip leading spaces
class SkipSpaces :: Symbol -> Symbol -> Constraint
class SkipSpaces sym result | sym -> result

instance SkipSpaces "" ""
else instance
  ( Symbol.Cons head tail sym
  , SkipSpacesGo head tail result
  ) =>
  SkipSpaces sym result

class SkipSpacesGo :: Symbol -> Symbol -> Symbol -> Constraint
class SkipSpacesGo head tail result | head tail -> result

instance SkipSpaces tail result => SkipSpacesGo " " tail result
else instance Symbol.Cons head tail result => SkipSpacesGo head tail result

-- Skip a string literal: consume chars until closing single quote
class SkipStringLiteral :: Symbol -> Symbol -> Constraint
class SkipStringLiteral sym rest | sym -> rest

instance Fail (Text "Unclosed string literal") => SkipStringLiteral "" rest
else instance
  ( Symbol.Cons h t sym
  , SkipStringLiteralGo h t rest
  ) =>
  SkipStringLiteral sym rest

class SkipStringLiteralGo :: Symbol -> Symbol -> Symbol -> Constraint
class SkipStringLiteralGo head tail rest | head tail -> rest

-- Closing quote: done
instance SkipStringLiteralGo "'" tail tail
-- End of string without closing quote
else instance Fail (Text "Unclosed string literal") => SkipStringLiteralGo h "" rest
-- Any other char: skip and continue
else instance
  ( Symbol.Cons nextH nextT tail
  , SkipStringLiteralGo nextH nextT rest
  ) =>
  SkipStringLiteralGo h tail rest

-- Extract the first word from a Symbol (up to space or end)
class ExtractWord :: Symbol -> Symbol -> Symbol -> Constraint
class ExtractWord sym word rest | sym -> word rest

instance ExtractWord "" "" ""
else instance
  ( Symbol.Cons h t sym
  , ExtractWordGo h t "" word rest
  ) =>
  ExtractWord sym word rest

class ExtractWordGo :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ExtractWordGo head tail acc word rest | head tail acc -> word rest

-- Stop on space
instance (SkipSpaces tail rest) => ExtractWordGo " " tail acc acc rest
-- Stop on comma (keep comma in rest)
else instance Symbol.Cons "," tail rest => ExtractWordGo "," tail acc acc rest
-- End of string
else instance Symbol.Append acc h word => ExtractWordGo h "" acc word ""
-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ExtractWordGo nextH nextT acc' word rest
  ) =>
  ExtractWordGo h tail acc word rest

-- StripColumns: (name :: String # Unique, ...) -> (name :: String, ...)
class StripColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class StripColumnsRL rl out | rl -> out

instance StripColumnsRL RL.Nil RL.Nil
instance (ExtractType entry typ, StripColumnsRL tail out') => StripColumnsRL (RL.Cons name entry tail) (RL.Cons name typ out')

class StripColumns :: Row Type -> Row Type -> Constraint
class StripColumns cols result | cols -> result

instance (RowToList cols rl, StripColumnsRL rl outRL, ListToRow outRL result) => StripColumns cols result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SingleTable: extract name and cols from a single-entry tables row
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SingleTable :: Row (Row Type) -> Symbol -> Row Type -> Constraint
class SingleTable tables name cols | tables -> name cols

instance
  ( RowToList tables (RL.Cons name cols RL.Nil)
  , IsSymbol name
  ) =>
  SingleTable tables name cols

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SplitOnDot: "users.id" -> ("users", "id"); "name" -> unqualified
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SplitOnDot :: Symbol -> Boolean -> Symbol -> Symbol -> Constraint
class SplitOnDot sym hasDot table col | sym -> hasDot table col

instance
  ( Symbol.Cons h t sym
  , SplitOnDotGo h t "" hasDot table col
  ) =>
  SplitOnDot sym hasDot table col

class SplitOnDotGo :: Symbol -> Symbol -> Symbol -> Boolean -> Symbol -> Symbol -> Constraint
class SplitOnDotGo head tail acc hasDot table col | head tail acc -> hasDot table col

-- Found dot: acc is table, rest is column
instance SplitOnDotGo "." tail acc True acc tail

-- End of string without dot: unqualified
else instance
  ( Symbol.Append acc h col
  ) =>
  SplitOnDotGo h "" acc False "" col

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , SplitOnDotGo nextH nextT acc' hasDot table col
  ) =>
  SplitOnDotGo h tail acc hasDot table col

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ResolveColumn: look up a column (qualified or unqualified) in tables
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ResolveColumn :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveColumn word tables typ | word tables -> typ

instance
  ( SplitOnDot word hasDot table col
  , ResolveColumnBranch hasDot table col tables typ
  ) =>
  ResolveColumn word tables typ

class ResolveColumnBranch :: Boolean -> Symbol -> Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveColumnBranch hasDot table col tables typ | hasDot table col tables -> typ

-- Qualified: has dot
instance
  ( Row.Cons table tableCols restTables tables
  , Row.Cons col typ restCols tableCols
  ) =>
  ResolveColumnBranch True table col tables typ

-- Unqualified: no dot
else instance
  ( RowToList tables tablesRL
  , FindUnqualifiedColumn col tablesRL typ
  ) =>
  ResolveColumnBranch False table col tables typ

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- FindUnqualifiedColumn: search all tables for a column name
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class FindUnqualifiedColumn :: Symbol -> RL.RowList (Row Type) -> Type -> Constraint
class FindUnqualifiedColumn col tablesRL typ | col tablesRL -> typ

instance
  ( Fail (Beside (Text "Column ") (Beside (Quote col) (Text " not found in any table")))
  ) =>
  FindUnqualifiedColumn col RL.Nil typ

instance
  ( RowToList tableCols colsRL
  , HasColumnRL col colsRL found
  , FindUnqualifiedColumnDecide found col tableName tableCols tailTables typ
  ) =>
  FindUnqualifiedColumn col (RL.Cons tableName tableCols tailTables) typ

class HasColumnRL :: Symbol -> RL.RowList Type -> Boolean -> Constraint
class HasColumnRL col rl found | col rl -> found

instance HasColumnRL col RL.Nil False
instance HasColumnRL col (RL.Cons col typ tail) True
else instance HasColumnRL col tail found => HasColumnRL col (RL.Cons name typ tail) found

class FindUnqualifiedColumnDecide :: Boolean -> Symbol -> Symbol -> Row Type -> RL.RowList (Row Type) -> Type -> Constraint
class FindUnqualifiedColumnDecide found col tableName tableCols restTables typ | found col tableName tableCols restTables -> typ

-- Found in this table: verify it's not in remaining tables
instance
  ( Row.Cons col typ restCols tableCols
  , AssertNotInRemainingTables col restTables
  ) =>
  FindUnqualifiedColumnDecide True col tableName tableCols restTables typ

-- Not found in this table: keep searching
instance
  FindUnqualifiedColumn col restTables typ =>
  FindUnqualifiedColumnDecide False col tableName tableCols restTables typ

class AssertNotInRemainingTables :: Symbol -> RL.RowList (Row Type) -> Constraint
class AssertNotInRemainingTables col tablesRL

instance AssertNotInRemainingTables col RL.Nil
instance
  ( RowToList tableCols colsRL
  , HasColumnRL col colsRL found
  , AssertNotAmbiguous found col tableName
  , AssertNotInRemainingTables col tail
  ) =>
  AssertNotInRemainingTables col (RL.Cons tableName tableCols tail)

class AssertNotAmbiguous :: Boolean -> Symbol -> Symbol -> Constraint
class AssertNotAmbiguous found col tableName

instance AssertNotAmbiguous False col tableName
instance
  ( Fail (Beside (Text "Column ") (Beside (Quote col) (Text " is ambiguous - qualify with table name")))
  ) =>
  AssertNotAmbiguous True col tableName

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseSelect: parse "users.name, posts.title AS t" against tables
-- Result labels: column name (after dot) or explicit AS alias
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseSelect :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseSelect sym tables result | sym tables -> result

instance ParseSelect "" tables ()
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" tables RL.Nil outRL
  , ListToRow outRL result
  ) =>
  ParseSelect sym tables result

class ParseSelectGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectGo head tail acc tables accRL outRL | head tail acc tables accRL -> outRL

-- Leading/double comma: no column name before comma
instance
  Fail (Text "Unexpected comma in SELECT clause (missing column name)") =>
  ParseSelectGo "," tail "" tables accRL outRL

-- Comma: emit column, continue
else instance
  ( ResolveColumn acc tables entry
  , ExtractType entry typ
  , SplitOnDot acc _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectGo "," tail acc tables accRL outRL

-- Space: column reference done, check for AS or comma
else instance
  ( SkipSpaces tail rest
  , ParseSelectAfterCol acc rest tables accRL outRL
  ) =>
  ParseSelectGo " " tail acc tables accRL outRL

-- Open paren: aggregate function call
else instance
  ( ExtractUntilParen tail args afterParen
  , ResolveAggregateArg args tables argType
  , AggregateReturnType acc argType returnType
  , SkipSpaces afterParen rest
  , ParseAfterAggregate rest tables returnType accRL outRL
  ) =>
  ParseSelectGo "(" tail acc tables accRL outRL

-- End of string: emit final column
else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables entry
  , ExtractType entry typ
  , SplitOnDot acc' _hasDot _table colName
  ) =>
  ParseSelectGo h "" acc tables accRL (RL.Cons colName typ accRL)

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseSelectGo nextH nextT acc' tables accRL outRL
  ) =>
  ParseSelectGo h tail acc tables accRL outRL

-- After column name + space: AS alias, comma, or end
class ParseSelectAfterCol :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterCol colRef rest tables accRL outRL | colRef rest tables accRL -> outRL

-- End: emit column with default label (column name after dot)
instance
  ( ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SplitOnDot colRef _hasDot _table colName
  ) =>
  ParseSelectAfterCol colRef "" tables accRL (RL.Cons colName typ accRL)

-- Non-empty: branch on first char
else instance
  ( Symbol.Cons h t rest
  , ParseSelectAfterColByHead h t colRef tables accRL outRL
  ) =>
  ParseSelectAfterCol colRef rest tables accRL outRL

class ParseSelectAfterColByHead :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterColByHead head tail colRef tables accRL outRL | head tail colRef tables accRL -> outRL

-- Comma: emit column with default label, continue
instance
  ( ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SplitOnDot colRef _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectAfterColByHead "," tail colRef tables accRL outRL

-- Otherwise (AS ...): extract word
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest keyword afterKeyword
  , ParseSelectHandleAS keyword afterKeyword colRef tables accRL outRL
  ) =>
  ParseSelectAfterColByHead h t colRef tables accRL outRL

class ParseSelectHandleAS :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectHandleAS keyword afterKeyword colRef tables accRL outRL | keyword afterKeyword colRef tables accRL -> outRL

instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "AS" afterKeyword colRef tables accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "as" afterKeyword colRef tables accRL outRL

class ParseSelectExpectEnd :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectEnd sym tables accRL outRL | sym tables accRL -> outRL

instance ParseSelectExpectEnd "" tables accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectExpectEndByHead h t tables accRL outRL
  ) =>
  ParseSelectExpectEnd sym tables accRL outRL

class ParseSelectExpectEndByHead :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectEndByHead head tail tables accRL outRL | head tail tables accRL -> outRL

instance
  ( SkipSpaces tail rest
  , ParseSelectContinue rest tables accRL outRL
  ) =>
  ParseSelectExpectEndByHead "," tail tables accRL outRL

class ParseSelectContinue :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectContinue sym tables accRL outRL | sym tables accRL -> outRL

instance Fail (Text "Trailing comma in SELECT clause") => ParseSelectContinue "" tables accRL outRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" tables accRL outRL
  ) =>
  ParseSelectContinue sym tables accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Aggregate functions in SELECT: COUNT(*), SUM(col), etc.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Extract characters until closing paren
class ExtractUntilParen :: Symbol -> Symbol -> Symbol -> Constraint
class ExtractUntilParen tail args afterParen | tail -> args afterParen

instance
  Fail (Text "Unclosed parenthesis in SELECT clause") =>
  ExtractUntilParen "" args afterParen

else instance
  ( Symbol.Cons h t tail
  , ExtractUntilParenGo h t "" Z args afterParen
  ) =>
  ExtractUntilParen tail args afterParen

-- Peano depth counter for nested parentheses
data Z

data S :: Type -> Type
data S n

class ExtractUntilParenGo :: Symbol -> Symbol -> Symbol -> Type -> Symbol -> Symbol -> Constraint
class ExtractUntilParenGo head tail acc depth args afterParen | head tail acc depth -> args afterParen

-- ) at depth zero: done
instance ExtractUntilParenGo ")" tail acc Z acc tail

-- ) at depth > 0: decrement depth, keep going
else instance
  ( Symbol.Append acc ")" acc'
  , Symbol.Cons nextH nextT tail
  , ExtractUntilParenGo nextH nextT acc' n args afterParen
  ) =>
  ExtractUntilParenGo ")" tail acc (S n) args afterParen

-- ( : increment depth, keep going
else instance
  ( Symbol.Append acc "(" acc'
  , Symbol.Cons nextH nextT tail
  , ExtractUntilParenGo nextH nextT acc' (S depth) args afterParen
  ) =>
  ExtractUntilParenGo "(" tail acc depth args afterParen

-- Unclosed paren at end of input
else instance
  Fail (Text "Unclosed parenthesis in SELECT clause") =>
  ExtractUntilParenGo h "" acc depth args afterParen

-- Any other char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ExtractUntilParenGo nextH nextT acc' depth args afterParen
  ) =>
  ExtractUntilParenGo h tail acc depth args afterParen

-- Resolve aggregate argument: "*" -> Star, column ref -> unwrapped type
data Star

class ResolveAggregateArg :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArg args tables argType | args tables -> argType

instance ResolveAggregateArg "*" tables Star
else instance ResolveAggregateArg "" tables Star
else instance
  ( SkipSpaces args trimmedFront
  , ExtractWord trimmedFront col _rest
  , ResolveAggregateArgCol col tables argType
  ) =>
  ResolveAggregateArg args tables argType

class ResolveAggregateArgCol :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArgCol col tables argType | col tables -> argType

instance ResolveAggregateArgCol "*" tables Star
else instance
  ( Symbol.Cons h _t col
  , ResolveAggregateArgColByHead h col tables argType
  ) =>
  ResolveAggregateArgCol col tables argType

class ResolveAggregateArgColByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArgColByHead head col tables argType | head col tables -> argType

-- Numeric literals: treat as Star (return type determined by function)
instance ResolveAggregateArgColByHead "0" col tables Star
else instance ResolveAggregateArgColByHead "1" col tables Star
else instance ResolveAggregateArgColByHead "2" col tables Star
else instance ResolveAggregateArgColByHead "3" col tables Star
else instance ResolveAggregateArgColByHead "4" col tables Star
else instance ResolveAggregateArgColByHead "5" col tables Star
else instance ResolveAggregateArgColByHead "6" col tables Star
else instance ResolveAggregateArgColByHead "7" col tables Star
else instance ResolveAggregateArgColByHead "8" col tables Star
else instance ResolveAggregateArgColByHead "9" col tables Star
-- Column reference
else instance
  ( ResolveColumn col tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  ResolveAggregateArgColByHead head col tables unwrapped

-- Map (funcName, argType) -> returnType
-- Dispatches on first character to avoid linear search
class AggregateReturnType :: Symbol -> Type -> Type -> Constraint
class AggregateReturnType funcName argType returnType | funcName argType -> returnType

instance
  ( Symbol.Cons head rest funcName
  , AggregateReturnTypeByHead head funcName argType returnType
  ) =>
  AggregateReturnType funcName argType returnType

class AggregateReturnTypeByHead :: Symbol -> Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeByHead head funcName argType returnType | head funcName argType -> returnType

instance AggregateReturnTypeA funcName argType returnType => AggregateReturnTypeByHead "A" funcName argType returnType
else instance AggregateReturnTypeA funcName argType returnType => AggregateReturnTypeByHead "a" funcName argType returnType
else instance AggregateReturnTypeC funcName argType returnType => AggregateReturnTypeByHead "C" funcName argType returnType
else instance AggregateReturnTypeC funcName argType returnType => AggregateReturnTypeByHead "c" funcName argType returnType
else instance AggregateReturnTypeD funcName argType returnType => AggregateReturnTypeByHead "D" funcName argType returnType
else instance AggregateReturnTypeD funcName argType returnType => AggregateReturnTypeByHead "d" funcName argType returnType
else instance AggregateReturnTypeF funcName argType returnType => AggregateReturnTypeByHead "F" funcName argType returnType
else instance AggregateReturnTypeF funcName argType returnType => AggregateReturnTypeByHead "f" funcName argType returnType
else instance AggregateReturnTypeG funcName argType returnType => AggregateReturnTypeByHead "G" funcName argType returnType
else instance AggregateReturnTypeG funcName argType returnType => AggregateReturnTypeByHead "g" funcName argType returnType
else instance AggregateReturnTypeL funcName argType returnType => AggregateReturnTypeByHead "L" funcName argType returnType
else instance AggregateReturnTypeL funcName argType returnType => AggregateReturnTypeByHead "l" funcName argType returnType
else instance AggregateReturnTypeM funcName argType returnType => AggregateReturnTypeByHead "M" funcName argType returnType
else instance AggregateReturnTypeM funcName argType returnType => AggregateReturnTypeByHead "m" funcName argType returnType
else instance AggregateReturnTypeN funcName argType returnType => AggregateReturnTypeByHead "N" funcName argType returnType
else instance AggregateReturnTypeN funcName argType returnType => AggregateReturnTypeByHead "n" funcName argType returnType
else instance AggregateReturnTypeR funcName argType returnType => AggregateReturnTypeByHead "R" funcName argType returnType
else instance AggregateReturnTypeR funcName argType returnType => AggregateReturnTypeByHead "r" funcName argType returnType
else instance AggregateReturnTypeS funcName argType returnType => AggregateReturnTypeByHead "S" funcName argType returnType
else instance AggregateReturnTypeS funcName argType returnType => AggregateReturnTypeByHead "s" funcName argType returnType
else instance AggregateReturnTypeV funcName argType returnType => AggregateReturnTypeByHead "V" funcName argType returnType
else instance AggregateReturnTypeV funcName argType returnType => AggregateReturnTypeByHead "v" funcName argType returnType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeByHead head funcName argType returnType

-- A: AVG
class AggregateReturnTypeA :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeA funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeA "AVG" argType Number
else instance AggregateReturnTypeA "avg" argType Number
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeA funcName argType returnType

-- C: COUNT, COALESCE
class AggregateReturnTypeC :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeC funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeC "COUNT" argType Int
else instance AggregateReturnTypeC "count" argType Int
else instance AggregateReturnTypeC "COALESCE" argType argType
else instance AggregateReturnTypeC "coalesce" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeC funcName argType returnType

-- D: DENSE_RANK
class AggregateReturnTypeD :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeD funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeD "DENSE_RANK" argType Int
else instance AggregateReturnTypeD "dense_rank" argType Int
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeD funcName argType returnType

-- F: FIRST_VALUE
class AggregateReturnTypeF :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeF funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeF "FIRST_VALUE" argType argType
else instance AggregateReturnTypeF "first_value" argType argType
else instance AggregateReturnTypeF "FLOOR" argType argType
else instance AggregateReturnTypeF "floor" argType argType
else instance AggregateReturnTypeF "FTS_SCORE" argType Number
else instance AggregateReturnTypeF "fts_score" argType Number
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeF funcName argType returnType

-- G: GROUP_CONCAT
class AggregateReturnTypeG :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeG funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeG "GROUP_CONCAT" argType String
else instance AggregateReturnTypeG "group_concat" argType String
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeG funcName argType returnType

-- L: LAG, LEAD, LAST_VALUE
class AggregateReturnTypeL :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeL funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeL "LAG" argType argType
else instance AggregateReturnTypeL "lag" argType argType
else instance AggregateReturnTypeL "LEAD" argType argType
else instance AggregateReturnTypeL "lead" argType argType
else instance AggregateReturnTypeL "LAST_VALUE" argType argType
else instance AggregateReturnTypeL "last_value" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeL funcName argType returnType

-- M: MIN, MAX
class AggregateReturnTypeM :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeM funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeM "MIN" argType argType
else instance AggregateReturnTypeM "min" argType argType
else instance AggregateReturnTypeM "MAX" argType argType
else instance AggregateReturnTypeM "max" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeM funcName argType returnType

-- N: NTILE, NTH_VALUE
class AggregateReturnTypeN :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeN funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeN "NTILE" argType Int
else instance AggregateReturnTypeN "ntile" argType Int
else instance AggregateReturnTypeN "NTH_VALUE" argType argType
else instance AggregateReturnTypeN "nth_value" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeN funcName argType returnType

-- R: ROW_NUMBER, RANK
class AggregateReturnTypeR :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeR funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeR "ROW_NUMBER" argType Int
else instance AggregateReturnTypeR "row_number" argType Int
else instance AggregateReturnTypeR "RANK" argType Int
else instance AggregateReturnTypeR "rank" argType Int
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeR funcName argType returnType

-- S: SUM
class AggregateReturnTypeS :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeS funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeS "SUM" argType argType
else instance AggregateReturnTypeS "sum" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeS funcName argType returnType

-- V: vector_extract, vector_distance_cos
class AggregateReturnTypeV :: Symbol -> Type -> Type -> Constraint
class AggregateReturnTypeV funcName argType returnType | funcName argType -> returnType

instance AggregateReturnTypeV "vector_extract" argType String
else instance AggregateReturnTypeV "VECTOR_EXTRACT" argType String
else instance AggregateReturnTypeV "vector_distance_cos" argType Number
else instance AggregateReturnTypeV "VECTOR_DISTANCE_COS" argType Number
else instance AggregateReturnTypeV "vector_distance_l2" argType Number
else instance AggregateReturnTypeV "VECTOR_DISTANCE_L2" argType Number
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnTypeV funcName argType returnType

-- After aggregate ): require AS alias, then continue
class ParseAfterAggregate :: Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregate rest tables returnType accRL outRL | rest tables returnType accRL -> outRL

-- End of string without alias
instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregate "" tables returnType accRL outRL

else instance
  ( Symbol.Cons h t rest
  , ParseAfterAggregateByHead h t tables returnType accRL outRL
  ) =>
  ParseAfterAggregate rest tables returnType accRL outRL

class ParseAfterAggregateByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregateByHead head tail tables returnType accRL outRL | head tail tables returnType accRL -> outRL

-- Comma without alias
instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregateByHead "," tail tables returnType accRL outRL

-- Otherwise: extract keyword and dispatch (OVER or AS)
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest keyword afterKeyword
  , ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL
  ) =>
  ParseAfterAggregateByHead h t tables returnType accRL outRL

class ParseAfterAggregateKeyword :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL | keyword afterKeyword tables returnType accRL -> outRL

-- OVER: parse the over clause, then continue to AS
instance
  ( SkipSpaces afterKeyword rest
  , ParseOverClause rest tables returnType accRL outRL
  ) =>
  ParseAfterAggregateKeyword "OVER" afterKeyword tables returnType accRL outRL

else instance
  ( SkipSpaces afterKeyword rest
  , ParseOverClause rest tables returnType accRL outRL
  ) =>
  ParseAfterAggregateKeyword "over" afterKeyword tables returnType accRL outRL

-- AS: extract alias
else instance
  ( ExtractWord afterKeyword alias afterAlias
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias returnType accRL) outRL
  ) =>
  ParseAfterAggregateKeyword "AS" afterKeyword tables returnType accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias returnType accRL) outRL
  ) =>
  ParseAfterAggregateKeyword "as" afterKeyword tables returnType accRL outRL

else instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL

-- Parse OVER (...) clause: expect (, skip until ), then continue to AS alias
class ParseOverClause :: Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseOverClause rest tables returnType accRL outRL | rest tables returnType accRL -> outRL

instance
  Fail (Text "Expected ( after OVER") =>
  ParseOverClause "" tables returnType accRL outRL

else instance
  ( Symbol.Cons h t rest
  , ParseOverClauseByHead h t tables returnType accRL outRL
  ) =>
  ParseOverClause rest tables returnType accRL outRL

class ParseOverClauseByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseOverClauseByHead head tail tables returnType accRL outRL | head tail tables returnType accRL -> outRL

-- Open paren: extract until close paren, validate content, continue to AS
instance
  ( ExtractUntilParen tail overContent afterParen
  , ValidateOverContent overContent tables
  , SkipSpaces afterParen rest
  , ParseAfterAggregate rest tables returnType accRL outRL
  ) =>
  ParseOverClauseByHead "(" tail tables returnType accRL outRL

else instance
  Fail (Text "Expected ( after OVER") =>
  ParseOverClauseByHead h t tables returnType accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateOverContent: validates PARTITION BY / ORDER BY inside OVER(...)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateOverContent :: Symbol -> Row (Row Type) -> Constraint
class ValidateOverContent content tables

instance ValidateOverContent "" tables

else instance
  ( SkipSpaces content content'
  , ValidateOverContentTrimmed content' tables
  ) =>
  ValidateOverContent content tables

class ValidateOverContentTrimmed :: Symbol -> Row (Row Type) -> Constraint
class ValidateOverContentTrimmed content tables

instance ValidateOverContentTrimmed "" tables

else instance
  ( ExtractWord content keyword rest
  , ValidateOverKeyword keyword rest tables
  ) =>
  ValidateOverContentTrimmed content tables

class ValidateOverKeyword :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOverKeyword keyword rest tables

instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidatePartitionByColumns afterBy tables
  ) =>
  ValidateOverKeyword "PARTITION" rest tables

else instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidatePartitionByColumns afterBy tables
  ) =>
  ValidateOverKeyword "partition" rest tables

else instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidateOrderBy afterBy tables
  ) =>
  ValidateOverKeyword "ORDER" rest tables

else instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidateOrderBy afterBy tables
  ) =>
  ValidateOverKeyword "order" rest tables

else instance
  Fail (Text "Expected PARTITION BY or ORDER BY inside OVER clause") =>
  ValidateOverKeyword keyword rest tables

class AssertIsBy :: Symbol -> Constraint
class AssertIsBy sym

instance AssertIsBy "BY"
else instance AssertIsBy "by"
else instance Fail (Text "Expected BY after PARTITION or ORDER") => AssertIsBy sym

-- ValidatePartitionByColumns: column list that terminates at ORDER keyword
class ValidatePartitionByColumns :: Symbol -> Row (Row Type) -> Constraint
class ValidatePartitionByColumns sym tables

instance ValidatePartitionByColumns "" tables

else instance
  ( SkipSpaces sym sym'
  , ValidatePartitionByColumnsTrimmed sym' tables
  ) =>
  ValidatePartitionByColumns sym tables

class ValidatePartitionByColumnsTrimmed :: Symbol -> Row (Row Type) -> Constraint
class ValidatePartitionByColumnsTrimmed sym tables

instance ValidatePartitionByColumnsTrimmed "" tables

else instance
  ( Symbol.Cons h t sym
  , ValidatePartitionByColumnsGo h t "" tables
  ) =>
  ValidatePartitionByColumnsTrimmed sym tables

class ValidatePartitionByColumnsGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidatePartitionByColumnsGo head tail acc tables

-- Comma: flush column, continue
instance
  ( ResolveColumn acc tables typ
  , SkipSpaces tail rest
  , ValidatePartitionByColumnsTrimmed rest tables
  ) =>
  ValidatePartitionByColumnsGo "," tail acc tables

-- Space: flush word, check if ORDER keyword or column
else instance
  ( SkipSpaces tail rest
  , FlushPartitionWord acc rest tables
  ) =>
  ValidatePartitionByColumnsGo " " tail acc tables

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables typ
  ) =>
  ValidatePartitionByColumnsGo h "" acc tables

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidatePartitionByColumnsGo nextH nextT acc' tables
  ) =>
  ValidatePartitionByColumnsGo h tail acc tables

class FlushPartitionWord :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushPartitionWord word rest tables

instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidateOrderBy afterBy tables
  ) =>
  FlushPartitionWord "ORDER" rest tables

else instance
  ( ExtractWord rest byWord afterBy
  , AssertIsBy byWord
  , ValidateOrderBy afterBy tables
  ) =>
  FlushPartitionWord "order" rest tables

else instance
  ( ResolveColumn word tables typ
  , ValidatePartitionByColumnsTrimmed rest tables
  ) =>
  FlushPartitionWord word rest tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseWhere: parse "id = $id AND age > $age" -> params ($ replaced with ? at runtime)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data NoType

class ParseWhere :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseWhere sym tables params | sym tables -> params

instance Fail (Text "Empty WHERE clause") => ParseWhere "" tables params
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" NoType tables RL.Nil outRL
  , ListToRow outRL params
  ) =>
  ParseWhere sym tables params

class ParseWhereGo :: Symbol -> Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereGo head tail acc currentType tables paramsIn paramsOut | head tail acc currentType tables paramsIn -> paramsOut

-- Space: flush word, continue
instance
  ( FlushWhereWord acc currentType tables paramsIn currentType' paramsOut'
  , SkipSpaces tail rest
  , ParseWhereContinue rest currentType' tables paramsOut' paramsOut
  ) =>
  ParseWhereGo " " tail acc currentType tables paramsIn paramsOut

-- Operators
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "=" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ">" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "<" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "!" tail acc currentType tables paramsIn paramsOut
else instance
  ( IsMultiArgFunc acc isMultiArg funcReturnType
  , ParseWhereFunc isMultiArg acc funcReturnType tail currentType tables paramsIn currentTypeOut paramsOut
  ) =>
  ParseWhereGo "(" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ")" tail acc currentType tables paramsIn paramsOut
-- String literal: flush word, skip until closing quote
else instance
  ( FlushWhereWord acc currentType tables paramsIn currentType' paramsOut'
  , SkipStringLiteral tail rest
  , ParseWhereContinue rest currentType' tables paramsOut' paramsOut
  ) =>
  ParseWhereGo "'" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "@" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "?" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ":" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "~" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "#" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "," tail acc currentType tables paramsIn paramsOut

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushWhereWord acc' currentType tables paramsIn _ct paramsOut
  ) =>
  ParseWhereGo h "" acc currentType tables paramsIn paramsOut

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseWhereGo nextH nextT acc' currentType tables paramsIn paramsOut
  ) =>
  ParseWhereGo h tail acc currentType tables paramsIn paramsOut

class ParseWhereContinue :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereContinue sym currentType tables paramsIn paramsOut | sym currentType tables paramsIn -> paramsOut

instance ParseWhereContinue "" currentType tables paramsIn paramsIn
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" currentType tables paramsIn paramsOut
  ) =>
  ParseWhereContinue sym currentType tables paramsIn paramsOut

-- Flush a word in WHERE context
-- Dispatches on first character to avoid linear search through all keywords
class FlushWhereWord :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWord word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWord "" currentType tables paramsIn currentType paramsIn
-- Most common SQL keywords matched directly for fast resolution
else instance FlushWhereWord "AND" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "and" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "OR" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "or" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "NOT" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "not" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "IS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "is" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "NULL" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "null" currentType tables paramsIn currentType paramsIn
-- Everything else: dispatch on first character
else instance
  ( Symbol.Cons head rest word
  , FlushWhereWordByHead head word currentType tables paramsIn currentTypeOut paramsOut
  ) =>
  FlushWhereWord word currentType tables paramsIn currentTypeOut paramsOut

class FlushWhereWordByHead :: Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordByHead head word currentType tables paramsIn currentTypeOut paramsOut | head word currentType tables paramsIn -> currentTypeOut paramsOut

-- $param: emit with currentType
instance
  ( Symbol.Cons "$" paramName word
  ) =>
  FlushWhereWordByHead "$" word currentType tables paramsIn currentType (RL.Cons paramName currentType paramsIn)

-- Digit: number literal, pass through
else instance FlushWhereWordByHead "0" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "1" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "2" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "3" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "4" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "5" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "6" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "7" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "8" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "9" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "*" word currentType tables paramsIn currentType paramsIn

-- Letters with keywords: dispatch to per-letter helpers
else instance FlushWhereWordA word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "A" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordA word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "a" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordB word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "B" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordB word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "b" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordC word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "C" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordC word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "c" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordD word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "D" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordD word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "d" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordE word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "E" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordE word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "e" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordF word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "F" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordF word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "f" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordG word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "G" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordG word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "g" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordI word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "I" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordI word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "i" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordJ word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "J" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordJ word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "j" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordL word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "L" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordL word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "l" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordM word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "M" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordM word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "m" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordN word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "N" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordN word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "n" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordS word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "S" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordS word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "s" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordT word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "T" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordT word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "t" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordU word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "U" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordU word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "u" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordV word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "V" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordV word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "v" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordP word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "P" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordP word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "p" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordR word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "R" word currentType tables paramsIn currentTypeOut paramsOut
else instance FlushWhereWordR word currentType tables paramsIn currentTypeOut paramsOut => FlushWhereWordByHead "r" word currentType tables paramsIn currentTypeOut paramsOut

-- Catch-all: column reference (letters without keywords like g, h, q, w, x, y, z)
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordByHead head word currentType tables paramsIn unwrapped paramsIn

-- Per-letter keyword helpers

class FlushWhereWordA :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordA word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordA "ABS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordA "abs" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordA "AS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordA "as" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordA "AVG" currentType tables paramsIn Number paramsIn
else instance FlushWhereWordA "avg" currentType tables paramsIn Number paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordA word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordB :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordB word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordB "BETWEEN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordB "between" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordB "bigint" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordB "boolean" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordB word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordC :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordC word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordC "CAST" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "cast" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "CEIL" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "ceil" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "COALESCE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "coalesce" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "CONCAT" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "concat" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "COUNT" currentType tables paramsIn Int paramsIn
else instance FlushWhereWordC "count" currentType tables paramsIn Int paramsIn
else instance FlushWhereWordC "CURRENT_DATE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "current_date" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "CURRENT_TIMESTAMP" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordC "current_timestamp" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordC word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordD :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordD word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordD "DATE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordD "date" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordD word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordE :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordE word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordE "EXISTS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordE "exists" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordE word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordF :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordF word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordF "FALSE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordF "false" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordF "FLOOR" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordF "floor" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordF "FTS_SCORE" currentType tables paramsIn Number paramsIn
else instance FlushWhereWordF "fts_score" currentType tables paramsIn Number paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordF word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordG :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordG word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordG "GROUP_CONCAT" currentType tables paramsIn String paramsIn
else instance FlushWhereWordG "group_concat" currentType tables paramsIn String paramsIn
else instance FlushWhereWordG "GREATEST" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordG "greatest" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordG "GLOB" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordG "glob" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordG word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordI :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordI word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordI "IN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordI "in" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordI "integer" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordI "int" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordI word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordJ :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordJ word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordJ "json" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJ "JSON" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordJ word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordL :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordL word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordL "LEAST" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordL "least" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordL "LENGTH" currentType tables paramsIn Int paramsIn
else instance FlushWhereWordL "length" currentType tables paramsIn Int paramsIn
else instance FlushWhereWordL "LIKE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordL "like" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordL "LOWER" currentType tables paramsIn String paramsIn
else instance FlushWhereWordL "lower" currentType tables paramsIn String paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordL word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordM :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordM word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordM "MIN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordM "min" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordM "MAX" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordM "max" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordM "MATCH" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordM "match" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordM word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordN :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordN word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordN "NOT" currentType tables paramsIn currentType paramsIn -- unreachable (caught by FlushWhereWord directly) but included for self-containedness
else instance FlushWhereWordN "not" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "NOW" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "now" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "NULL" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "null" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "NULLIF" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordN "nullif" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordN word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordS :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordS word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordS "SMALLINT" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordS "smallint" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordS "SUM" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordS "sum" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordS word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordT :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordT word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordT "TRIM" currentType tables paramsIn String paramsIn
else instance FlushWhereWordT "trim" currentType tables paramsIn String paramsIn
else instance FlushWhereWordT "TRUE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "true" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "text" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "timestamp" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "TIME" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "time" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordT "TYPEOF" currentType tables paramsIn String paramsIn
else instance FlushWhereWordT "typeof" currentType tables paramsIn String paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordT word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordU :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordU word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordU "UPPER" currentType tables paramsIn String paramsIn
else instance FlushWhereWordU "upper" currentType tables paramsIn String paramsIn
else instance FlushWhereWordU "uuid" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordU word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordV :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordV word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordV "varchar" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordV "vector_extract" currentType tables paramsIn String paramsIn
else instance FlushWhereWordV "VECTOR_EXTRACT" currentType tables paramsIn String paramsIn
else instance FlushWhereWordV "vector_distance_cos" currentType tables paramsIn Number paramsIn
else instance FlushWhereWordV "VECTOR_DISTANCE_COS" currentType tables paramsIn Number paramsIn
else instance FlushWhereWordV "vector_distance_l2" currentType tables paramsIn Number paramsIn
else instance FlushWhereWordV "VECTOR_DISTANCE_L2" currentType tables paramsIn Number paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordV word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordP :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordP word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordP "PRINTF" currentType tables paramsIn String paramsIn
else instance FlushWhereWordP "printf" currentType tables paramsIn String paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordP word currentType tables paramsIn unwrapped paramsIn

class FlushWhereWordR :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordR word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordR "real" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordR "REAL" currentType tables paramsIn currentType paramsIn
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordR word currentType tables paramsIn unwrapped paramsIn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- IsMultiArgFunc: maps function names to Boolean + return type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsMultiArgFunc :: Symbol -> Boolean -> Type -> Constraint
class IsMultiArgFunc funcName isMultiArg returnType | funcName -> isMultiArg returnType

instance IsMultiArgFunc "vector_distance_cos" True Number
else instance IsMultiArgFunc "VECTOR_DISTANCE_COS" True Number
else instance IsMultiArgFunc "vector_distance_l2" True Number
else instance IsMultiArgFunc "VECTOR_DISTANCE_L2" True Number
else instance IsMultiArgFunc "vector_extract" True String
else instance IsMultiArgFunc "VECTOR_EXTRACT" True String
else instance IsMultiArgFunc "fts_score" True Number
else instance IsMultiArgFunc "FTS_SCORE" True Number
else instance IsMultiArgFunc other False NoType

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseWhereFunc: dispatch on IsMultiArgFunc result
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseWhereFunc :: Boolean -> Symbol -> Type -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class ParseWhereFunc isMultiArg funcName funcReturnType tail currentType tables paramsIn currentTypeOut paramsOut
  | isMultiArg funcName funcReturnType tail currentType tables paramsIn -> currentTypeOut paramsOut

instance
  ( ExtractUntilParen tail funcArgs afterParen
  , ParseWhereContinue funcArgs NoType tables paramsIn paramsOut'
  , SkipSpaces afterParen afterParenTrimmed
  , ParseWhereContinue afterParenTrimmed funcReturnType tables paramsOut' paramsOut
  ) =>
  ParseWhereFunc True funcName funcReturnType tail currentType tables paramsIn funcReturnType paramsOut

else instance
  ( FlushWhereWord funcName currentType tables paramsIn currentType' paramsOut'
  , ParseWhereContinue tail currentType' tables paramsOut' paramsOut
  ) =>
  ParseWhereFunc False funcName _returnType tail currentType tables paramsIn currentType' paramsOut

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateColumnList: comma-separated column references
-- Used by GROUP BY, DISTINCT ON, ON CONFLICT target
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateColumnList :: Symbol -> Row (Row Type) -> Constraint
class ValidateColumnList sym tables

instance Fail (Text "Empty column list") => ValidateColumnList "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnListGo h t "" tables
  ) =>
  ValidateColumnList sym tables

class ValidateColumnListContinue :: Symbol -> Row (Row Type) -> Constraint
class ValidateColumnListContinue sym tables

instance ValidateColumnListContinue "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnListGo h t "" tables
  ) =>
  ValidateColumnListContinue sym tables

class ValidateColumnListGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateColumnListGo head tail acc tables

instance
  ( ResolveColumn acc tables typ
  , SkipSpaces tail rest
  , ValidateColumnListContinue rest tables
  ) =>
  ValidateColumnListGo "," tail acc tables

else instance
  ( SkipSpaces tail rest
  , ResolveColumn acc tables typ
  , ValidateColumnListContinue rest tables
  ) =>
  ValidateColumnListGo " " tail acc tables

else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables typ
  ) =>
  ValidateColumnListGo h "" acc tables

else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnListGo nextH nextT acc' tables
  ) =>
  ValidateColumnListGo h tail acc tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateOrderBy: validate ORDER BY with qualified column support
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateOrderBy :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderBy sym tables

instance ValidateOrderBy "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByGo h t "" tables
  ) =>
  ValidateOrderBy sym tables

class ValidateOrderByGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByGo head tail acc tables

-- Leading/double comma: no column before comma
instance
  Fail (Text "Unexpected comma in ORDER BY clause (missing column name)") =>
  ValidateOrderByGo "," tail "" tables

-- Comma: flush column, continue (non-empty acc)
else instance
  ( FlushOrderByWord acc tables
  , SkipSpaces tail rest
  , ValidateOrderByContinueNonEmpty rest tables
  ) =>
  ValidateOrderByGo "," tail acc tables

-- Space: flush column, skip modifiers
else instance
  ( SkipSpaces tail rest
  , FlushOrderByThenSkip acc rest tables
  ) =>
  ValidateOrderByGo " " tail acc tables

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , FlushOrderByFinalWord acc' tables
  ) =>
  ValidateOrderByGo h "" acc tables

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateOrderByGo nextH nextT acc' tables
  ) =>
  ValidateOrderByGo h tail acc tables

-- After comma: reject empty (trailing comma)
class ValidateOrderByContinueNonEmpty :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByContinueNonEmpty sym tables

instance Fail (Text "Trailing comma in ORDER BY clause") => ValidateOrderByContinueNonEmpty "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByGo h t "" tables
  ) =>
  ValidateOrderByContinueNonEmpty sym tables

-- End of string: reject bare modifiers (ASC/DESC without column)
class FlushOrderByFinalWord :: Symbol -> Row (Row Type) -> Constraint
class FlushOrderByFinalWord word tables

instance Fail (Text "ORDER BY requires at least one column") => FlushOrderByFinalWord "" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "ASC" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "asc" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "DESC" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "desc" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "NULLS" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "FIRST" tables
else instance Fail (Text "ORDER BY requires a column name before ASC/DESC") => FlushOrderByFinalWord "LAST" tables
else instance ResolveColumn word tables typ => FlushOrderByFinalWord word tables

class FlushOrderByWord :: Symbol -> Row (Row Type) -> Constraint
class FlushOrderByWord word tables

instance FlushOrderByWord "" tables
else instance FlushOrderByWord "ASC" tables
else instance FlushOrderByWord "asc" tables
else instance FlushOrderByWord "DESC" tables
else instance FlushOrderByWord "desc" tables
else instance FlushOrderByWord "NULLS" tables
else instance FlushOrderByWord "FIRST" tables
else instance FlushOrderByWord "LAST" tables
else instance ResolveColumn word tables typ => FlushOrderByWord word tables

class FlushOrderByThenSkip :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByThenSkip colName rest tables

instance FlushOrderByWord colName tables => FlushOrderByThenSkip colName "" tables
else instance
  ( FlushOrderByWord colName tables
  , Symbol.Cons h t rest
  , FlushOrderByThenSkipByHead h t tables
  ) =>
  FlushOrderByThenSkip colName rest tables

class FlushOrderByThenSkipByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByThenSkipByHead head tail tables

-- Comma: continue with next column
instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest tables
  ) =>
  FlushOrderByThenSkipByHead "," tail tables

-- Modifier word: consume it, continue
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' tables
  ) =>
  FlushOrderByThenSkipByHead h t tables

class ValidateOrderByContinue :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByContinue sym tables

instance ValidateOrderByContinue "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByContinueByHead h t tables
  ) =>
  ValidateOrderByContinue sym tables

class ValidateOrderByContinueByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByContinueByHead head tail tables

instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest tables
  ) =>
  ValidateOrderByContinueByHead "," tail tables

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' tables
  ) =>
  ValidateOrderByContinueByHead h t tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- RowListHas: check if a label exists in a RowList
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class RowListHas :: Symbol -> RL.RowList Type -> Boolean -> Constraint
class RowListHas label rl has | label rl -> has

instance RowListHas label RL.Nil False
instance RowListHas label (RL.Cons label typ rest) True
else instance RowListHas label rest has => RowListHas label (RL.Cons other typ rest) has

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- CheckDistinctOrderBy: when DISTINCT is in stage, ORDER BY
-- columns must appear in the SELECT result list
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class CheckDistinctOrderBy :: Row Type -> Symbol -> Row Type -> Constraint
class CheckDistinctOrderBy stage cols result

instance
  ( RowToList stage stageRL
  , RowListHas "distinct" stageRL isDistinct
  , CheckDistinctOrderByBranch isDistinct cols result
  ) =>
  CheckDistinctOrderBy stage cols result

class CheckDistinctOrderByBranch :: Boolean -> Symbol -> Row Type -> Constraint
class CheckDistinctOrderByBranch isDistinct cols result

instance OrderByColumnsInResult cols result => CheckDistinctOrderByBranch True cols result
instance CheckDistinctOrderByBranch False cols result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- OrderByColumnsInResult: validate ORDER BY column refs exist in result row
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class OrderByColumnsInResult :: Symbol -> Row Type -> Constraint
class OrderByColumnsInResult sym result

instance OrderByColumnsInResult "" result
else instance
  ( Symbol.Cons h t sym
  , OrderByColumnsInResultGo h t "" result
  ) =>
  OrderByColumnsInResult sym result

class OrderByColumnsInResultGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class OrderByColumnsInResultGo head tail acc result

-- Comma: flush column, continue
instance
  ( FlushOrderByWordInResult acc result
  , SkipSpaces tail rest
  , OrderByColumnsInResult rest result
  ) =>
  OrderByColumnsInResultGo "," tail acc result

-- Space: flush word, skip modifiers
else instance
  ( SkipSpaces tail rest
  , FlushOrderByThenSkipInResult acc rest result
  ) =>
  OrderByColumnsInResultGo " " tail acc result

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , FlushOrderByWordInResult acc' result
  ) =>
  OrderByColumnsInResultGo h "" acc result

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , OrderByColumnsInResultGo nextH nextT acc' result
  ) =>
  OrderByColumnsInResultGo h tail acc result

class FlushOrderByWordInResult :: Symbol -> Row Type -> Constraint
class FlushOrderByWordInResult word result

instance FlushOrderByWordInResult "" result
else instance FlushOrderByWordInResult "ASC" result
else instance FlushOrderByWordInResult "asc" result
else instance FlushOrderByWordInResult "DESC" result
else instance FlushOrderByWordInResult "desc" result
else instance FlushOrderByWordInResult "NULLS" result
else instance FlushOrderByWordInResult "FIRST" result
else instance FlushOrderByWordInResult "LAST" result
else instance CheckColumnInResult word result => FlushOrderByWordInResult word result

class CheckColumnInResult :: Symbol -> Row Type -> Constraint
class CheckColumnInResult col result

instance
  ( RowToList result rl
  , CheckColumnInResultRL col rl
  ) =>
  CheckColumnInResult col result

class CheckColumnInResultRL :: Symbol -> RL.RowList Type -> Constraint
class CheckColumnInResultRL col rl

instance
  Fail (Beside (Text "ORDER BY column ") (Beside (Quote col) (Text " must appear in the SELECT list when using DISTINCT"))) =>
  CheckColumnInResultRL col RL.Nil

instance CheckColumnInResultRL col (RL.Cons col typ tail)

else instance CheckColumnInResultRL col tail => CheckColumnInResultRL col (RL.Cons name typ tail)

class FlushOrderByThenSkipInResult :: Symbol -> Symbol -> Row Type -> Constraint
class FlushOrderByThenSkipInResult colName rest result

instance FlushOrderByWordInResult colName result => FlushOrderByThenSkipInResult colName "" result
else instance
  ( FlushOrderByWordInResult colName result
  , Symbol.Cons h t rest
  , FlushOrderByThenSkipByHeadInResult h t result
  ) =>
  FlushOrderByThenSkipInResult colName rest result

class FlushOrderByThenSkipByHeadInResult :: Symbol -> Symbol -> Row Type -> Constraint
class FlushOrderByThenSkipByHeadInResult head tail result

-- Comma: continue with next column
instance
  ( SkipSpaces tail rest
  , OrderByColumnsInResult rest result
  ) =>
  FlushOrderByThenSkipByHeadInResult "," tail result

-- Modifier word: consume it, continue
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWordInResult word result
  , SkipSpaces afterWord rest'
  , OrderByColumnsInResultContinue rest' result
  ) =>
  FlushOrderByThenSkipByHeadInResult h t result

class OrderByColumnsInResultContinue :: Symbol -> Row Type -> Constraint
class OrderByColumnsInResultContinue sym result

instance OrderByColumnsInResultContinue "" result
else instance
  ( Symbol.Cons h t sym
  , OrderByColumnsInResultContinueByHead h t result
  ) =>
  OrderByColumnsInResultContinue sym result

class OrderByColumnsInResultContinueByHead :: Symbol -> Symbol -> Row Type -> Constraint
class OrderByColumnsInResultContinueByHead head tail result

instance
  ( SkipSpaces tail rest
  , OrderByColumnsInResult rest result
  ) =>
  OrderByColumnsInResultContinueByHead "," tail result

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWordInResult word result
  , SkipSpaces afterWord rest'
  , OrderByColumnsInResultContinue rest' result
  ) =>
  OrderByColumnsInResultContinueByHead h t result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateColumns: comma-separated column names (for ON CONFLICT target)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateColumns :: Symbol -> Row Type -> Constraint
class ValidateColumns sym cols

instance ValidateColumns "" cols
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnsGo h t "" cols
  ) =>
  ValidateColumns sym cols

class ValidateColumnsGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class ValidateColumnsGo head tail acc cols

-- Comma: flush name, continue
instance
  ( FlushColumnWord acc cols
  , SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateColumnsGo "," tail acc cols

-- Space: flush name, skip to comma or end
else instance
  ( SkipSpaces tail rest
  , ValidateAfterName acc rest cols
  ) =>
  ValidateColumnsGo " " tail acc cols

-- End of string: flush final name
else instance
  ( Symbol.Append acc h acc'
  , FlushColumnWord acc' cols
  ) =>
  ValidateColumnsGo h "" acc cols

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnsGo nextH nextT acc' cols
  ) =>
  ValidateColumnsGo h tail acc cols

class FlushColumnWord :: Symbol -> Row Type -> Constraint
class FlushColumnWord word cols

instance FlushColumnWord "" cols
else instance FlushColumnWord "$" cols
else instance FlushColumnWord "AND" cols
else instance FlushColumnWord "OR" cols
else instance FlushColumnWord "NOT" cols
else instance FlushColumnWord "IS" cols
else instance FlushColumnWord "NULL" cols
else instance FlushColumnWord "LIKE" cols
else instance FlushColumnWord "IN" cols
else instance FlushColumnWord "TRUE" cols
else instance FlushColumnWord "FALSE" cols
else instance FlushColumnWord "BETWEEN" cols
else instance
  ( Symbol.Cons head rest word
  , FlushColumnWordByHead head word cols
  ) =>
  FlushColumnWord word cols

class FlushColumnWordByHead :: Symbol -> Symbol -> Row Type -> Constraint
class FlushColumnWordByHead head word cols

instance FlushColumnWordByHead "$" word cols
else instance FlushColumnWordByHead "0" word cols
else instance FlushColumnWordByHead "1" word cols
else instance FlushColumnWordByHead "2" word cols
else instance FlushColumnWordByHead "3" word cols
else instance FlushColumnWordByHead "4" word cols
else instance FlushColumnWordByHead "5" word cols
else instance FlushColumnWordByHead "6" word cols
else instance FlushColumnWordByHead "7" word cols
else instance FlushColumnWordByHead "8" word cols
else instance FlushColumnWordByHead "9" word cols
else instance Row.Cons word typ rest cols => FlushColumnWordByHead head word cols
else instance
  Fail (Beside (Beside (Text "Column ") (Quote word)) (Text " does not exist in the table")) =>
  FlushColumnWordByHead head word cols

class ValidateAfterName :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterName acc rest cols

instance FlushColumnWord acc cols => ValidateAfterName acc "" cols
else instance
  ( FlushColumnWord acc cols
  , Symbol.Cons h t rest
  , ValidateAfterNameByHead h t cols
  ) =>
  ValidateAfterName acc rest cols

class ValidateAfterNameByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterNameByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateAfterNameByHead "," tail cols

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , HandleAfterColumnWord word afterWord cols
  ) =>
  ValidateAfterNameByHead h t cols

class HandleAfterColumnWord :: Symbol -> Symbol -> Row Type -> Constraint
class HandleAfterColumnWord word rest cols

instance SkipAlias rest cols => HandleAfterColumnWord "AS" rest cols
else instance SkipAlias rest cols => HandleAfterColumnWord "as" rest cols

class SkipAlias :: Symbol -> Row Type -> Constraint
class SkipAlias sym cols

instance SkipAlias "" cols
else instance
  ( ExtractWord sym _alias afterAlias
  , SkipSpaces afterAlias rest
  , ExpectCommaOrEnd rest cols
  ) =>
  SkipAlias sym cols

class ExpectCommaOrEnd :: Symbol -> Row Type -> Constraint
class ExpectCommaOrEnd sym cols

instance ExpectCommaOrEnd "" cols
else instance
  ( Symbol.Cons h t sym
  , ExpectCommaOrEndByHead h t cols
  ) =>
  ExpectCommaOrEnd sym cols

class ExpectCommaOrEndByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ExpectCommaOrEndByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ExpectCommaOrEndByHead "," tail cols

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateJoinCondition: validate ON clause column references
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateJoinCondition :: Symbol -> Row (Row Type) -> Constraint
class ValidateJoinCondition sym tables

instance ValidateJoinCondition "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateJoinCondGo h t "" tables
  ) =>
  ValidateJoinCondition sym tables

class ValidateJoinCondGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateJoinCondGo head tail acc tables

-- Space: flush word, continue
instance
  ( FlushJoinWord acc tables
  , SkipSpaces tail rest
  , ValidateJoinCondition rest tables
  ) =>
  ValidateJoinCondGo " " tail acc tables

-- Operators
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "=" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo ">" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "<" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "!" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "(" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo ")" tail acc tables

-- String literal: skip content between quotes
else instance
  ( FlushJoinWord acc tables
  , SkipStringLiteral tail rest
  , ValidateJoinCondition rest tables
  ) =>
  ValidateJoinCondGo "'" tail acc tables

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushJoinWord acc' tables
  ) =>
  ValidateJoinCondGo h "" acc tables

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateJoinCondGo nextH nextT acc' tables
  ) =>
  ValidateJoinCondGo h tail acc tables

class FlushJoinWord :: Symbol -> Row (Row Type) -> Constraint
class FlushJoinWord word tables

instance FlushJoinWord "" tables
else instance FlushJoinWord "AND" tables
else instance FlushJoinWord "OR" tables
else instance FlushJoinWord "NOT" tables
else instance FlushJoinWord "IS" tables
else instance FlushJoinWord "NULL" tables
else instance FlushJoinWord "TRUE" tables
else instance FlushJoinWord "FALSE" tables
else instance
  ( Symbol.Cons head rest word
  , FlushJoinWordByHead head word tables
  ) =>
  FlushJoinWord word tables

class FlushJoinWordByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushJoinWordByHead head word tables

instance FlushJoinWordByHead "$" word tables
else instance FlushJoinWordByHead "0" word tables
else instance FlushJoinWordByHead "1" word tables
else instance FlushJoinWordByHead "2" word tables
else instance FlushJoinWordByHead "3" word tables
else instance FlushJoinWordByHead "4" word tables
else instance FlushJoinWordByHead "5" word tables
else instance FlushJoinWordByHead "6" word tables
else instance FlushJoinWordByHead "7" word tables
else instance FlushJoinWordByHead "8" word tables
else instance FlushJoinWordByHead "9" word tables
else instance ResolveColumn word tables typ => FlushJoinWordByHead head word tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ON CONFLICT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseConflictAction :: Symbol -> Row Type -> Constraint
class ParseConflictAction sym cols

instance
  ( ExtractWord sym w1 rest1
  , ExpectKeyword w1 "DO"
  , ExtractWord rest1 w2 rest2
  , ExpectKeyword w2 "UPDATE"
  , ExtractWord rest2 w3 rest3
  , ExpectKeyword w3 "SET"
  , ParseAssignments rest3 cols
  ) =>
  ParseConflictAction sym cols

class ExpectKeyword :: Symbol -> Symbol -> Constraint
class ExpectKeyword actual expected

instance ExpectKeyword a a
else instance
  Fail (Beside (Beside (Text "Expected keyword ") (Quote expected)) (Beside (Text " but got ") (Quote actual))) =>
  ExpectKeyword actual expected

class ParseAssignments :: Symbol -> Row Type -> Constraint
class ParseAssignments sym cols

instance ParseAssignments "" cols
else instance
  ( ExtractWord sym colName rest1
  , Row.Cons colName colType colRest cols
  , SkipSpaces rest1 rest2
  , ExpectChar rest2 "=" rest3
  , SkipSpaces rest3 rest4
  , ExtractWord rest4 excRef rest5
  , ValidateExcludedRef excRef colName
  , SkipSpaces rest5 rest6
  , ParseAssignmentsContinue rest6 cols
  ) =>
  ParseAssignments sym cols

class ParseAssignmentsContinue :: Symbol -> Row Type -> Constraint
class ParseAssignmentsContinue sym cols

instance ParseAssignmentsContinue "" cols
else instance
  ( Symbol.Cons h t sym
  , ParseAssignmentsContinueByHead h t cols
  ) =>
  ParseAssignmentsContinue sym cols

class ParseAssignmentsContinueByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ParseAssignmentsContinueByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ParseAssignments rest cols
  ) =>
  ParseAssignmentsContinueByHead "," tail cols

class ExpectChar :: Symbol -> Symbol -> Symbol -> Constraint
class ExpectChar sym char rest | sym char -> rest

instance
  ( Symbol.Cons h t sym
  , ExpectCharMatch h t char rest
  ) =>
  ExpectChar sym char rest

class ExpectCharMatch :: Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ExpectCharMatch head tail expected rest | head tail expected -> rest

instance ExpectCharMatch c tail c tail
else instance
  Fail (Beside (Beside (Text "Expected '") (Quote expected)) (Beside (Text "' but got '") (Quote head))) =>
  ExpectCharMatch head tail expected rest

class ValidateExcludedRef :: Symbol -> Symbol -> Constraint
class ValidateExcludedRef ref colName

instance
  ( Symbol.Append "EXCLUDED." colName expected
  , MatchSymbol ref expected
  ) =>
  ValidateExcludedRef ref colName

class MatchSymbol :: Symbol -> Symbol -> Constraint
class MatchSymbol a b

instance MatchSymbol a a
else instance
  Fail (Beside (Beside (Text "Expected ") (Quote expected)) (Beside (Text " but got ") (Quote actual))) =>
  MatchSymbol actual expected

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-level IsAutoGenerated (Boolean kind)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsAutoGeneratedTC :: Type -> Boolean -> Constraint
class IsAutoGeneratedTC constraints result | constraints -> result

instance IsAutoGeneratedTC (AutoIncrement a) True
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (Default s a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (DefaultExpr expr a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (PrimaryKey a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (Unique a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (ForeignKey t r c a) result
else instance IsAutoGeneratedTC (RandomRowId a) True
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (Nullable a) result
else instance IsAutoGeneratedTC a False

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- InsertableColumnsRL: filter out auto-generated columns
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class InsertableColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class InsertableColumnsRL tableRL outRL | tableRL -> outRL

instance InsertableColumnsRL RL.Nil RL.Nil
instance
  ( IsAutoGeneratedTC entry isAuto
  , ExtractType entry typ
  , InsertableColumnDecide isAuto name typ tail outRL
  ) =>
  InsertableColumnsRL (RL.Cons name entry tail) outRL

class InsertableColumnDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class InsertableColumnDecide isAuto name typ tail outRL | isAuto name typ tail -> outRL

instance InsertableColumnsRL tail outRL => InsertableColumnDecide True name typ tail outRL
instance InsertableColumnsRL tail outRL => InsertableColumnDecide False name typ tail (RL.Cons name typ outRL)

-- IsOptionalForInsertTC: check if a column is optional for insert
-- (auto-generated, has default, or is Maybe)
class IsOptionalForInsertTC :: Type -> Boolean -> Constraint
class IsOptionalForInsertTC entry result | entry -> result

instance IsOptionalForInsertTC (AutoIncrement a) True
else instance IsOptionalForInsertTC (RandomRowId a) True
else instance IsOptionalForInsertTC (Default s a) True
else instance IsOptionalForInsertTC (DefaultExpr e a) True
else instance IsOptionalForInsertTC a result => IsOptionalForInsertTC (PrimaryKey a) result
else instance IsOptionalForInsertTC a result => IsOptionalForInsertTC (Unique a) result
else instance IsOptionalForInsertTC a result => IsOptionalForInsertTC (ForeignKey t r c a) result
else instance IsOptionalForInsertTC a result => IsOptionalForInsertTC (Nullable a) result
else instance IsOptionalForInsertTC (Maybe a) True
else instance IsOptionalForInsertTC a False

-- RequiredForInsertRL: compute required insert columns from original table RowList
class RequiredForInsertRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class RequiredForInsertRL tableRL outRL | tableRL -> outRL

instance RequiredForInsertRL RL.Nil RL.Nil
instance
  ( IsOptionalForInsertTC entry isOptional
  , ExtractType entry typ
  , RequiredForInsertDecide isOptional name typ tail outRL
  ) =>
  RequiredForInsertRL (RL.Cons name entry tail) outRL

class RequiredForInsertDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class RequiredForInsertDecide isOptional name typ tail outRL | isOptional name typ tail -> outRL

instance RequiredForInsertRL tail outRL => RequiredForInsertDecide True name typ tail outRL
instance RequiredForInsertRL tail outRL => RequiredForInsertDecide False name typ tail (RL.Cons name typ outRL)

-- Generate column names from a RowList
class ColumnNamesRL :: RL.RowList Type -> Constraint
class ColumnNamesRL rl where
  columnNamesRL :: Proxy rl -> Array String

instance ColumnNamesRL RL.Nil where
  columnNamesRL _ = []

instance (IsSymbol name, ColumnNamesRL tail) => ColumnNamesRL (RL.Cons name typ tail) where
  columnNamesRL _ = [ reflectSymbol (Proxy :: Proxy name) ] <> columnNamesRL (Proxy :: Proxy tail)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- RecordValuesRL: extract record values in RowList order
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class FieldToSQLiteValue :: Type -> Constraint
class FieldToSQLiteValue a where
  fieldToSQLiteValue :: a -> SQLite.SQLiteValue

instance FieldToSQLiteValue a => FieldToSQLiteValue (Maybe a) where
  fieldToSQLiteValue = toNullable >>> unsafeCoerce
else instance FieldToSQLiteValue DateTime where
  fieldToSQLiteValue dt = unsafeCoerce (SQLite.dateTimeToString (JSDate.fromDateTime dt))
else instance FieldToSQLiteValue SQLDate where
  fieldToSQLiteValue (SQLDate d) = unsafeCoerce (SCU.take 10 (SQLite.dateTimeToString (JSDate.fromDateTime (DateTime d bottom))))
else instance FieldToSQLiteValue SQLTime where
  fieldToSQLiteValue (SQLTime t) = unsafeCoerce (pad (fromEnum (hour t)) <> ":" <> pad (fromEnum (minute t)) <> ":" <> pad (fromEnum (second t)))
    where
    pad n = if n < 10 then "0" <> show n else show n
else instance FieldToSQLiteValue Boolean where
  fieldToSQLiteValue b = unsafeCoerce (if b then 1 else 0)
else instance FieldToSQLiteValue SQLiteBool where
  fieldToSQLiteValue (SQLiteBool b) = unsafeCoerce (if b then 1 else 0)
else instance FieldToSQLiteValue Json where
  fieldToSQLiteValue (Json f) = unsafeCoerce (unsafeStringify f)
else instance FieldToSQLiteValue a where
  fieldToSQLiteValue = unsafeCoerce

class RecordValuesRL :: RL.RowList Type -> Row Type -> Constraint
class RecordValuesRL rl row where
  recordValuesRL :: Proxy rl -> { | row } -> Array SQLite.SQLiteValue

instance RecordValuesRL RL.Nil row where
  recordValuesRL _ _ = []

instance
  ( IsSymbol name
  , Row.Cons name typ rest row
  , FieldToSQLiteValue typ
  , RecordValuesRL tail row
  ) =>
  RecordValuesRL (RL.Cons name typ tail) row where
  recordValuesRL _ rec =
    [ fieldToSQLiteValue (Record.get (Proxy :: Proxy name) rec) ]
      <> recordValuesRL (Proxy :: Proxy tail) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- MakeNullableRL: wrap column types in Maybe for LEFT JOIN
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class MakeNullableRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class MakeNullableRL rl out | rl -> out

instance MakeNullableRL RL.Nil RL.Nil
instance
  ( ExtractType entry typ
  , MakeNullableDecide typ name entry tail out
  ) =>
  MakeNullableRL (RL.Cons name entry tail) out

class MakeNullableDecide :: Type -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class MakeNullableDecide typ name entry tail out | typ name entry tail -> out

-- Already nullable: keep as-is
instance MakeNullableRL tail out' => MakeNullableDecide (Maybe a) name entry tail (RL.Cons name entry out')
-- Not nullable: wrap in Nullable
else instance MakeNullableRL tail out' => MakeNullableDecide typ name entry tail (RL.Cons name (Nullable entry) out')

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query builder: unified Q type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype Q :: Row (Row Type) -> Row Type -> Row Type -> Row Type -> Type
newtype Q tables result params stage = Q { sql :: String, values :: Array SQLite.SQLiteValue }

class HasClause :: Symbol -> Row Type -> Constraint
class HasClause label row

instance Row.Cons label Unit rest row => HasClause label row

class HasAnyDML :: Row Type -> Constraint
class HasAnyDML stage

instance (RL.RowToList stage rl, HasAnyDMLRL rl) => HasAnyDML stage

class HasAnyDMLRL :: RL.RowList Type -> Constraint
class HasAnyDMLRL rl

instance HasAnyDMLRL (RL.Cons "select" Unit rest)
else instance HasAnyDMLRL (RL.Cons "set" Unit rest)
else instance HasAnyDMLRL (RL.Cons "delete" Unit rest)
else instance HasAnyDMLRL rest => HasAnyDMLRL (RL.Cons label typ rest)
else instance Fail (Text "WHERE requires a preceding SELECT, UPDATE (set), or DELETE") => HasAnyDMLRL RL.Nil

toSQL :: forall tables result params stage. Q tables result params stage -> String
toSQL (Q q) = q.sql

from :: forall name cols tables. IsSymbol name => Row.Cons name cols () tables => Proxy (Table name cols) -> Q tables () () ()
from _ = Q { sql: reflectSymbol (Proxy :: Proxy name), values: [] }

fromAs :: forall @alias name cols tables. IsSymbol name => IsSymbol alias => Row.Cons alias cols () tables => Proxy (Table name cols) -> Q tables () () ()
fromAs _ = Q { sql: reflectSymbol (Proxy :: Proxy name) <> " " <> reflectSymbol (Proxy :: Proxy alias), values: [] }

fromRaw :: forall @sql tables. IsSymbol sql => Q tables () () ()
fromRaw = Q { sql: reflectSymbol (Proxy :: Proxy sql), values: [] }

selectAll
  :: forall tables name cols result r p stage stage'
   . SingleTable tables name cols
  => IsSymbol name
  => StripColumns cols result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
selectAll (Q q) = Q (q { sql = "SELECT * FROM " <> q.sql })

select
  :: forall @sel tables result r p stage stage'
   . IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
select (Q q) = Q (q { sql = "SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

selectDistinct
  :: forall @sel tables result r p stage stage' stage''
   . IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Row.Cons "distinct" Unit stage' stage''
  => Q tables r p stage
  -> Q tables result p stage''
selectDistinct (Q q) = Q (q { sql = "SELECT DISTINCT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

selectRaw
  :: forall @sel @result tables r p stage stage'
   . IsSymbol sel
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
selectRaw (Q q) = Q (q { sql = "SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

where_
  :: forall @whr tables result params p stage stage'
   . IsSymbol whr
  => ParseWhere whr tables params
  => HasAnyDML stage
  => Row.Lacks "where" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "where" Unit stage stage'
  => Q tables result p stage
  -> Q tables result params stage'
where_ (Q q) = Q (q { sql = q.sql <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr) })

whereRaw
  :: forall @whr @params tables result p stage stage'
   . IsSymbol whr
  => HasAnyDML stage
  => Row.Lacks "where" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "where" Unit stage stage'
  => Q tables result p stage
  -> Q tables result params stage'
whereRaw (Q q) = Q (q { sql = q.sql <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr) })

orderBy
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => ValidateOrderBy cols tables
  => CheckDistinctOrderBy stage cols result
  => HasClause "select" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "orderBy" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
orderBy (Q q) = Q (q { sql = q.sql <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy cols) })

orderByRaw
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => HasClause "select" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "orderBy" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
orderByRaw (Q q) = Q (q { sql = q.sql <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy cols) })

groupBy
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => ValidateColumnList cols tables
  => HasClause "select" stage
  => Row.Lacks "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "groupBy" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
groupBy (Q q) = Q (q { sql = q.sql <> " GROUP BY " <> reflectSymbol (Proxy :: Proxy cols) })

having
  :: forall @cond tables result params havingParams allParams stage stage'
   . IsSymbol cond
  => ParseWhere cond tables havingParams
  => Row.Union params havingParams allParams
  => Row.Nub allParams allParams
  => HasClause "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "having" Unit stage stage'
  => Q tables result params stage
  -> Q tables result allParams stage'
having (Q q) = Q (q { sql = q.sql <> " HAVING " <> reflectSymbol (Proxy :: Proxy cond) })

class IsLimitParam :: Symbol -> Symbol -> Boolean -> Constraint
class IsLimitParam head tail isParam | head tail -> isParam

instance IsLimitParam "$" tail True
else instance IsLimitParam head tail False

class ParseLimitOffsetParams :: Boolean -> Symbol -> Row Type -> Row Type -> Constraint
class ParseLimitOffsetParams isParam name params params' | isParam name params -> params'

instance
  ( Row.Lacks name params
  , Row.Cons name Int params params'
  ) =>
  ParseLimitOffsetParams True name params params'

instance ParseLimitOffsetParams False name params params

-- Validate that a Symbol is a non-negative integer (all digits, non-empty)
class ValidateNumericLiteral :: Symbol -> Constraint
class ValidateNumericLiteral sym

instance
  ( Symbol.Cons h t sym
  , ValidateAllDigits h t
  ) =>
  ValidateNumericLiteral sym

class ValidateAllDigits :: Symbol -> Symbol -> Constraint
class ValidateAllDigits head tail

instance ValidateAllDigits "0" ""
else instance ValidateAllDigits "1" ""
else instance ValidateAllDigits "2" ""
else instance ValidateAllDigits "3" ""
else instance ValidateAllDigits "4" ""
else instance ValidateAllDigits "5" ""
else instance ValidateAllDigits "6" ""
else instance ValidateAllDigits "7" ""
else instance ValidateAllDigits "8" ""
else instance ValidateAllDigits "9" ""
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "0" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "1" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "2" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "3" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "4" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "5" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "6" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "7" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "8" tail
else instance (Symbol.Cons h2 t2 tail, ValidateAllDigits h2 t2) => ValidateAllDigits "9" tail
else instance
  Fail (Beside (Text "LIMIT/OFFSET must be a non-negative integer or $parameter, got: ") (Quote sym)) =>
  ValidateAllDigits head tail

class ParseLimitOffset :: Symbol -> Row Type -> Row Type -> Constraint
class ParseLimitOffset sym params params' | sym params -> params'

instance
  ( Symbol.Cons head tail sym
  , IsLimitParam head tail isParam
  , ParseLimitOffsetBranch isParam sym tail params params'
  ) =>
  ParseLimitOffset sym params params'

class ParseLimitOffsetBranch :: Boolean -> Symbol -> Symbol -> Row Type -> Row Type -> Constraint
class ParseLimitOffsetBranch isParam sym paramName params params' | isParam sym paramName params -> params'

instance
  ( Row.Lacks paramName params
  , Row.Cons paramName Int params params'
  ) =>
  ParseLimitOffsetBranch True sym paramName params params'

instance
  ValidateNumericLiteral sym =>
  ParseLimitOffsetBranch False sym paramName params params

limit
  :: forall @sym tables result params params' stage stage'
   . ParseLimitOffset sym params params'
  => IsSymbol sym
  => HasClause "select" stage
  => Row.Lacks "limit" stage
  => Row.Cons "limit" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params' stage'
limit (Q q) = Q (q { sql = q.sql <> " LIMIT " <> reflectSymbol (Proxy :: Proxy sym) })

offset
  :: forall @sym tables result params params' stage stage'
   . ParseLimitOffset sym params params'
  => IsSymbol sym
  => HasClause "select" stage
  => Row.Lacks "offset" stage
  => Row.Cons "offset" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params' stage'
offset (Q q) = Q (q { sql = q.sql <> " OFFSET " <> reflectSymbol (Proxy :: Proxy sym) })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- INSERT builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

insert
  :: forall tables name cols colsRL insertableRL insertable requiredRL required
       optionalProvided missing userRow userRowRL stage stage'
   . SingleTable tables name cols
  => RowToList cols colsRL
  => InsertableColumnsRL colsRL insertableRL
  => ListToRow insertableRL insertable
  => RequiredForInsertRL colsRL requiredRL
  => ListToRow requiredRL required
  => Row.Union required optionalProvided userRow
  => Row.Union userRow missing insertable
  => RowToList userRow userRowRL
  => ColumnNamesRL userRowRL
  => RecordValuesRL userRowRL userRow
  => IsSymbol name
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "insert" Unit stage stage'
  => { | userRow }
  -> Q tables () () stage
  -> Q tables () () stage'
insert rec _ = Q { sql, values }
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  colNames = columnNamesRL (Proxy :: Proxy userRowRL)
  placeholders = colNames # mapWithIndex \i _ -> "?" <> show (i + 1)
  sql =
    if Array.null colNames then "INSERT INTO " <> tableName <> " DEFAULT VALUES"
    else "INSERT INTO " <> tableName
      <> " ("
      <> intercalate ", " colNames
      <> ")"
      <> " VALUES ("
      <> intercalate ", " placeholders
      <> ")"
  values = recordValuesRL (Proxy :: Proxy userRowRL) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- RETURNING clause
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

returning
  :: forall @sel tables name cols result p stage stage'
   . SingleTable tables name cols
  => IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q tables () p stage
  -> Q tables result p stage'
returning (Q q) = Q (q { sql = q.sql <> " RETURNING " <> reflectSymbol (Proxy :: Proxy sel) })

returningAll
  :: forall tables name cols result p stage stage'
   . SingleTable tables name cols
  => StripColumns cols result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q tables () p stage
  -> Q tables result p stage'
returningAll (Q q) = Q (q { sql = q.sql <> " RETURNING *" })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UPDATE builder (SET)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateSetColumnsRL :: RL.RowList Type -> Row Type -> Constraint
class ValidateSetColumnsRL rl cols

instance ValidateSetColumnsRL RL.Nil cols
instance
  ( Row.Cons name entry rest cols
  , ExtractType entry typ
  , ValidateSetColumnsRL tail cols
  ) =>
  ValidateSetColumnsRL (RL.Cons name typ tail) cols

class SetClauseRL :: RL.RowList Type -> Constraint
class SetClauseRL rl where
  setClauseRL :: Proxy rl -> Int -> Array String

instance SetClauseRL RL.Nil where
  setClauseRL _ _ = []

instance (IsSymbol name, SetClauseRL tail) => SetClauseRL (RL.Cons name typ tail) where
  setClauseRL _ idx =
    [ reflectSymbol (Proxy :: Proxy name) <> " = ?" <> show idx ]
      <> setClauseRL (Proxy :: Proxy tail) (idx + 1)

set
  :: forall tables name cols setRow setRL stage stage'
   . SingleTable tables name cols
  => RowToList setRow setRL
  => IsSymbol name
  => ValidateSetColumnsRL setRL cols
  => SetClauseRL setRL
  => RecordValuesRL setRL setRow
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "set" Unit stage stage'
  => { | setRow }
  -> Q tables () () stage
  -> Q tables () () stage'
set rec _ = Q { sql, values }
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  setClauses = setClauseRL (Proxy :: Proxy setRL) 1
  sql = "UPDATE " <> tableName <> " SET " <> intercalate ", " setClauses
  values = recordValuesRL (Proxy :: Proxy setRL) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DELETE builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

delete
  :: forall tables name cols r p stage stage'
   . SingleTable tables name cols
  => IsSymbol name
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "delete" Unit stage stage'
  => Q tables r p stage
  -> Q tables () () stage'
delete _ = Q { sql: "DELETE FROM " <> reflectSymbol (Proxy :: Proxy name), values: [] }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ON CONFLICT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

onConflict
  :: forall @target @action tables name cols result params stage stage'
   . SingleTable tables name cols
  => IsSymbol target
  => IsSymbol action
  => ValidateColumns target cols
  => ParseConflictAction action cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
onConflict (Q q) = Q
  ( q
      { sql = q.sql
          <> " ON CONFLICT ("
          <> reflectSymbol (Proxy :: Proxy target)
          <> ") "
          <> reflectSymbol (Proxy :: Proxy action)
      }
  )

onConflictDoNothing
  :: forall @target tables name cols result params stage stage'
   . SingleTable tables name cols
  => IsSymbol target
  => ValidateColumns target cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
onConflictDoNothing (Q q) = Q
  ( q
      { sql = q.sql
          <> " ON CONFLICT ("
          <> reflectSymbol (Proxy :: Proxy target)
          <> ") DO NOTHING"
      }
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JOIN builders
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

innerJoin
  :: forall @cond name cols tables tables' r p stage
   . IsSymbol name
  => IsSymbol cond
  => Row.Lacks name tables
  => Row.Cons name cols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
innerJoin _ (Q q) = Q
  { sql: q.sql
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

leftJoin
  :: forall @cond name cols colsRL nullableColsRL nullableCols tables tables' r p stage
   . IsSymbol name
  => IsSymbol cond
  => RowToList cols colsRL
  => MakeNullableRL colsRL nullableColsRL
  => ListToRow nullableColsRL nullableCols
  => Row.Lacks name tables
  => Row.Cons name nullableCols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
leftJoin _ (Q q) = Q
  { sql: q.sql
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

innerJoinAs
  :: forall @alias @cond name cols tables tables' r p stage
   . IsSymbol name
  => IsSymbol alias
  => IsSymbol cond
  => Row.Lacks alias tables
  => Row.Cons alias cols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
innerJoinAs _ (Q q) = Q
  { sql: q.sql
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " "
      <> reflectSymbol (Proxy :: Proxy alias)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

leftJoinAs
  :: forall @alias @cond name cols colsRL nullableColsRL nullableCols tables tables' r p stage
   . IsSymbol name
  => IsSymbol alias
  => IsSymbol cond
  => RowToList cols colsRL
  => MakeNullableRL colsRL nullableColsRL
  => ListToRow nullableColsRL nullableCols
  => Row.Lacks alias tables
  => Row.Cons alias nullableCols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
leftJoinAs _ (Q q) = Q
  { sql: q.sql
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " "
      <> reflectSymbol (Proxy :: Proxy alias)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Set operations: UNION, INTERSECT, EXCEPT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type SetOpStage =
  ( select :: Unit
  , "where" :: Unit
  , groupBy :: Unit
  , having :: Unit
  , insert :: Unit
  , "set" :: Unit
  , "delete" :: Unit
  )

union
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
union (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") UNION (" <> q2.sql <> ")", values: q1.values <> q2.values }

unionAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
unionAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") UNION ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

intersect
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
intersect (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") INTERSECT (" <> q2.sql <> ")", values: q1.values <> q2.values }

intersectAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
intersectAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") INTERSECT ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

except_
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
except_ (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") EXCEPT (" <> q2.sql <> ")", values: q1.values <> q2.values }

exceptAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
exceptAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") EXCEPT ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query execution
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParamsToArray :: RL.RowList Type -> Row Type -> Constraint
class ParamsToArray rl row where
  paramsToArray :: Proxy rl -> { | row } -> Array { name :: String, value :: Foreign }

instance ParamsToArray RL.Nil row where
  paramsToArray _ _ = []

instance
  ( IsSymbol name
  , Row.Cons name typ rest row
  , Row.Lacks name rest
  , ParamsToArray tail row
  ) =>
  ParamsToArray (RL.Cons name typ tail) row where
  paramsToArray _ rec =
    [ { name: reflectSymbol (Proxy :: Proxy name)
      , value: unsafeToForeign (Record.get (Proxy :: Proxy name) rec)
      }
    ]
      <> paramsToArray (Proxy :: Proxy tail) rec

namedParamRegex :: Regex.Regex
namedParamRegex = case Regex.regex "\\$[a-zA-Z_][a-zA-Z0-9_]*" Regex.global of
  Right r -> r
  Left _ -> unsafeCoerce unit

replaceNamedParams :: Int -> Array { name :: String, value :: Foreign } -> String -> { sql :: String, values :: Array SQLite.SQLiteValue }
replaceNamedParams offset entries sql = do
  let indexed = entries # mapWithIndex \i e -> { idx: offset + i + 1, name: e.name, value: e.value }
  let replacements = indexed # foldl (\m e -> Map.insert ("$" <> e.name) ("?" <> show e.idx) m) Map.empty
  let
    sql' = Regex.replace' namedParamRegex
      ( \match _ -> case Map.lookup match replacements of
          Nothing -> match
          Just v -> v
      )
      sql
  { sql: sql', values: map (\e -> unsafeCoerce e.value) indexed }

runQuery
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => SQLite.Connection
  -> { | params }
  -> Q tables result params stage
  -> Aff (Array { | result })
runQuery conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- SQLite.query (SQLite.SQL sql) allValues conn
  case decodeRows result.rows of
    Left errs -> throwError (Exception.error ("Row decode failed: " <> show errs))
    Right rows -> pure rows

runQueryOne
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => SQLite.Connection
  -> { | params }
  -> Q tables result params stage
  -> Aff (Maybe { | result })
runQueryOne conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- SQLite.queryOne (SQLite.SQL sql) allValues conn
  case result of
    Nothing -> pure Nothing
    Just row -> case decodeRow row of
      Left errs -> throwError (Exception.error ("Row decode failed: " <> show errs))
      Right a -> pure (Just a)

runExecute
  :: forall tables params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => SQLite.Connection
  -> { | params }
  -> Q tables () params stage
  -> Aff Int
runExecute conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  SQLite.execute (SQLite.SQL sql) allValues conn

-- Transaction variants

runQueryTx
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => SQLite.Transaction
  -> { | params }
  -> Q tables result params stage
  -> Aff (Array { | result })
runQueryTx txn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- SQLite.txQuery (SQLite.SQL sql) allValues txn
  case decodeRows result.rows of
    Left errs -> throwError (Exception.error ("Row decode failed: " <> show errs))
    Right rows -> pure rows

runExecuteTx
  :: forall tables params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => SQLite.Transaction
  -> { | params }
  -> Q tables () params stage
  -> Aff Int
runExecuteTx txn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  SQLite.txExecute (SQLite.SQL sql) allValues txn

decodeRows :: forall a. ReadForeign a => Array Foreign -> Either (NonEmptyList ForeignError) (Array a)
decodeRows = traverse (readImpl >>> runExcept)

decodeRow :: forall a. ReadForeign a => Foreign -> Either (NonEmptyList ForeignError) a
decodeRow = readImpl >>> runExcept
