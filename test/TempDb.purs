module Test.Sqlite.TempDb where

import Effect (Effect)

foreign import mkTempDbUrl :: Effect String
