{-# LANGUAGE QuasiQuotes #-}
module Fixture(
    migrateFixture
  , deleteFixture
  ) where

import           Control.Monad
import           Database.PostgreSQL.Query
import           DB

-- | Migrate all data that is needed for tests
migrateFixture :: IO ()
migrateFixture = runDB $ do
  squareFunction
  testSchema
  squareFunctionSchema
  succAndPredFunc

-- | Delete fixture from DB
deleteFixture :: IO ()
deleteFixture = runDB $ do
  squareFunctionDrop
  squareFunctionSchemaDrop
  testSchemaDrop
  succAndPredFuncDrop

-- | Stored function for squaring input
squareFunction :: PostgresM ()
squareFunction = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION square1(a integer) RETURNS integer AS $$
  BEGIN
    RETURN a*a;
  END;
  $$ LANGUAGE plpgsql;
  |]

-- | Delete square function
squareFunctionDrop :: PostgresM ()
squareFunctionDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS square1(a integer);
  |]

-- | Create test schema
testSchema :: PostgresM ()
testSchema = void $ pgExecute [sqlExp|CREATE SCHEMA IF NOT EXISTS test;|]

-- | Delete test schema
testSchemaDrop :: PostgresM ()
testSchemaDrop = void $ pgExecute [sqlExp|DROP SCHEMA test;|]

-- | Stored function for squaring input
squareFunctionSchema :: PostgresM ()
squareFunctionSchema = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION test.square2(b integer) RETURNS integer AS $$
  BEGIN
    RETURN b*b;
  END;
  $$ LANGUAGE plpgsql;
  |]

-- | Delete square function
squareFunctionSchemaDrop :: PostgresM ()
squareFunctionSchemaDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS test.square2(b integer);
  |]

succAndPredFunc :: PostgresM ()
succAndPredFunc = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION "succAndPred"(n integer) RETURNS TABLE (a integer, b integer) AS $$
    SELECT n+1 as a, n-1 as b;
  $$ LANGUAGE sql;
  |]

succAndPredFuncDrop :: PostgresM ()
succAndPredFuncDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS "succAndPred"(n integer);
  |]
