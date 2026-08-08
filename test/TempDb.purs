module Test.Sqlite.TempDb where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)

foreign import mkTempDbUrl :: Effect String

foreign import testBytes :: Uint8Array

foreign import uint8ArrayValues :: Uint8Array -> Array Int
