{-# LANGUAGE QuasiQuotes #-}
module Fixture(
    migrateFixture
  , deleteFixture
  , module Reexport
  ) where

import           Control.Monad
import           Database.PostgreSQL.Query
import           DB

import           Fixture.User              as Reexport

-- | Migrate all data that is needed for tests
migrateFixture :: IO ()
migrateFixture = runDB $ do
  squareFunction
  testSchema
  squareFunctionSchema
  succAndPredFunc
  userFuncs

-- | Delete fixture from DB
deleteFixture :: IO ()
deleteFixture = runDB $ do
  squareFunctionDrop
  squareFunctionSchemaDrop
  testSchemaDrop
  succAndPredFuncDrop
  userFuncsDrop

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

userFuncs :: PostgresM ()
userFuncs = void $ pgExecute [sqlExp|
  CREATE TYPE "userCreate" AS(
    name text,
    password text,
    regTime date
  );

  CREATE TABLE IF NOT EXISTS "users"(
    id serial PRIMARY KEY,
    name text NOT NULL,
    password text NOT NULL,
    regTime date NOT NULL
  );

  CREATE OR REPLACE FUNCTION "postUser"(u "userCreate") RETURNS integer AS $$
    INSERT INTO users(name, password, regTime) VALUES (u.name, u.password, u.regTime)
    RETURNING id;
  $$ LANGUAGE sql;

  CREATE OR REPLACE FUNCTION "getUsers"() RETURNS SETOF users AS $$
    SELECT * FROM users;
  $$ LANGUAGE sql;
  |]

userFuncsDrop :: PostgresM ()
userFuncsDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS "postUser"(u "userCreate");
  DROP FUNCTION IF EXISTS "getUsers"();
  DROP TYPE IF EXISTS "userCreate" CASCADE;
  DROP TABLE IF EXISTS "users";
  |]
