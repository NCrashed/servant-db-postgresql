{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}
module Fixture(
    SquareAPI
  , SquareSchemaAPI
  , SuccAndPredAPI
  , UserAPI
  , OrderedAPI1
  , OrderedAPI2
  , VoidAPI
  , withSquareFunc
  , withTestSchema
  , withSquareSchema
  , withSuccAndPred
  , withUserFuncs
  , withOrderedFuncs
  , withVoid
  , module Reexport
  ) where

import           Control.Exception
import           Control.Monad
import           Database.PostgreSQL.Query
import           DB
import           Servant.API.DB
import           Servant.DB.PostgreSQL

import           Fixture.User              as Reexport

type SquareAPI = ArgNamed "a" Int :> Procedure "square1" (Only Int)
type SquareSchemaAPI = "test" :> ArgNamed "b" Int :> Procedure "square2" (Only Int)
type SuccAndPredAPI = ArgNamed "n" Int :> Procedure "succAndPred" (Int, Int)
type UserAPI =
       ArgNamed "u" (Composite UserCreate) :> Procedure "postUser" (Only Int)
  :<|> Procedure "getUsers" [User]
type OrderedAPI1 = ArgPos Int :> ArgPos Int :> ArgNamed "a" Int
  :> Procedure "ordered" (Only Int)
type OrderedAPI2 = ArgPos Int :> ArgNamed "a" Int :> ArgPos Int
  :> Procedure "ordered" (Only Int)
type VoidAPI = Procedure "void" ()

-- | Helper to make cleanable migration
withMigration :: PostgresM () -> PostgresM () -> IO c -> IO c
withMigration before after = bracket_ (runDB before) (runDB after)

withSquareFunc :: IO a -> IO a
withSquareFunc = withMigration squareFunction squareFunctionDrop

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

withTestSchema :: IO a -> IO a
withTestSchema = withMigration testSchema testSchemaDrop

-- | Create test schema
testSchema :: PostgresM ()
testSchema = void $ pgExecute [sqlExp|CREATE SCHEMA IF NOT EXISTS test;|]

-- | Delete test schema
testSchemaDrop :: PostgresM ()
testSchemaDrop = void $ pgExecute [sqlExp|DROP SCHEMA test;|]

withSquareSchema :: IO a -> IO a
withSquareSchema = withMigration squareFunctionSchema squareFunctionSchemaDrop

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

withSuccAndPred :: IO a -> IO a
withSuccAndPred = withMigration succAndPredFunc succAndPredFuncDrop

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

withUserFuncs :: IO a -> IO a
withUserFuncs = withMigration userFuncs userFuncsDrop

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

withOrderedFuncs :: IO a -> IO a
withOrderedFuncs = withMigration orderedFuncs orderedFuncsDrop

orderedFuncs :: PostgresM ()
orderedFuncs = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION "ordered"(integer, integer, a integer) RETURNS integer AS $$
  BEGIN
    RETURN $1 + $2*2 + a*3;
  END;
  $$ LANGUAGE plpgsql;
  |]

orderedFuncsDrop :: PostgresM ()
orderedFuncsDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS "ordered"(integer, integer, a integer);
 |]

withVoid :: IO a -> IO a
withVoid = withMigration voidFunc voidFuncDrop

voidFunc :: PostgresM ()
voidFunc = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION "void"() RETURNS void AS $$
  BEGIN
  END;
  $$ LANGUAGE plpgsql;
  |]

voidFuncDrop :: PostgresM ()
voidFuncDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS "void"();
  |]
