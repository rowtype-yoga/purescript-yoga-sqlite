-- EXPECT: No type class instance was found for
module Test.CompileFail.UnsupportedFieldValue where

import Prelude
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Yoga.SQLite.SQLite (Connection)
import Yoga.SQLite.Schema

data Unsupported = Unsupported

type UnsupportedTable = Table "unsupported_values"
  ( payload :: Unsupported
  )

unsupportedTable :: Proxy UnsupportedTable
unsupportedTable = Proxy

bad :: Connection -> Aff Int
bad conn = from unsupportedTable # insert { payload: Unsupported } # runExecute conn {}
