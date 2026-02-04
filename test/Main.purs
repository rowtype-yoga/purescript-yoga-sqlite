module Test.Sqlite.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.SQLite.SQLite as SQLite

setupSQLite :: Aff SQLite.DBConnection
setupSQLite = liftEffect $ SQLite.open (SQLite.DatabasePath ":memory:")

spec :: Spec Unit
spec = before setupSQLite do
  describe "Yoga.SQLite FFI" do
    describe "Basic Operations" do
      it "creates table and inserts data" \db -> do
        liftEffect $ SQLite.exec "CREATE TABLE test (id INTEGER PRIMARY KEY, name TEXT)" [] db
        liftEffect $ SQLite.exec "INSERT INTO test (name) VALUES ('Alice')" [] db
        pure unit

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec
