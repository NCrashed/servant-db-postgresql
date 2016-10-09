module Fixture(
    migrateFixture
  , deleteFixture
  ) where

import DB
import Database.PostgreSQL.Query
import Control.Monad

-- | Migrate all data that is needed for tests
migrateFixture :: IO ()
migrateFixture = runDB squareFunction

-- | Delete fixture from DB
deleteFixture :: IO ()
deleteFixture = runDB squareFunctionDrop

-- | Stored function for squaring input
squareFunction :: PostgresM ()
squareFunction = void $ pgExecute [sqlExp|
  CREATE OR REPLACE FUNCTION square(a integer) RETURNS integer AS $$
  BEGIN
    RETURN a*a;
  END;
  $$ LANGUAGE plpgsql;
  |]

-- | Delete square function
squareFunctionDrop :: PostgresM ()
squareFunctionDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS square(a integer);
  |]
