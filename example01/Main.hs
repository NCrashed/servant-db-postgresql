{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import           Control.Exception
import           Control.Monad             (void)
import           Control.Monad.Base
import           Control.Monad.Logger
import           Data.ByteString           (ByteString)
import           Data.Proxy
import           Data.Time
import           Database.PostgreSQL.Query
import           Servant.API.DB
import           Servant.DB.PostgreSQL

type UserId = Int

data RegisterUser = RegisterUser {
  userRegName     :: String
, userRegPassword :: String
, userRegRegTime  :: Day
} deriving (Eq)

deriveToRow ''RegisterUser

data User = User {
  userId       :: UserId
, userName     :: String
, userPassword :: String
, userRegTime  :: Day
} deriving (Eq)

deriveFromRow ''User
deriveToRow ''User

type UserAPI =
       ArgNamed "u" (Composite RegisterUser)
    :> Procedure "postUser" (Only Int)
  :<|> ArgPos Int
    :> Procedure "getUser" (Maybe User)
  :<|> ArgPos Int
    :> Procedure "deleteUser" ()
  :<|> Procedure "getUsers" [User]

newtype PostgresM a = PostgresM { runPostgresM :: PgMonadT (LoggingT IO) a }
  deriving (Functor, HasPostgres, MonadLogger, Monad, Applicative, MonadBase IO)

postUser :: Composite RegisterUser -> PostgresM (Only Int)
getUser :: Int -> PostgresM (Maybe User)
deleteUser :: Int -> PostgresM ()
getUsers :: PostgresM [User]
(      postUser
  :<|> getUser
  :<|> deleteUser
  :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)

main :: IO ()
main = withDB $ do
  t <- getCurrentTime
  let user = RegisterUser "Vasya" "123456" (utctDay t)
  Only i <- runDB $ postUser $ Composite user
  users <- runDB getUsers
  let user' = User {
          userId = i
        , userName = userRegName user
        , userPassword = userRegPassword user
        , userRegTime = userRegRegTime user
        }
  assertEqual "wrote != read list" [user'] users
  muser1 <- runDB $ getUser i
  assertEqual "wrote != read single" (Just user') muser1
  runDB $ deleteUser i
  muser2 <- runDB $ getUser i
  assertEqual "deleted cannot be read" Nothing muser2
  where
    assertEqual msg a b
      | a == b = return ()
      | otherwise = fail msg

dbConnection :: ByteString
dbConnection = "host=localhost dbname=servant_db_postgresql_test port=5432"

runDB :: PostgresM a -> IO a
runDB m = do
  con <- connectPostgreSQL dbConnection
  runStderrLoggingT . runPgMonadT con . runPostgresM $ m

withDB :: IO a -> IO a
withDB = bracket_ (runDB userFuncs) (runDB userFuncsDrop)

userFuncs :: PostgresM ()
userFuncs = void $ pgExecute [sqlExp|
  CREATE TYPE "userRegister" AS(
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

  CREATE OR REPLACE FUNCTION "postUser"(u "userRegister") RETURNS integer AS $$
    INSERT INTO users(name, password, regTime) VALUES (u.name, u.password, u.regTime)
    RETURNING id;
  $$ LANGUAGE sql;

  CREATE OR REPLACE FUNCTION "getUser"(int) RETURNS users AS $$
    SELECT * FROM users WHERE id = $1;
  $$ LANGUAGE sql;

  CREATE OR REPLACE FUNCTION "deleteUser"(int) RETURNS void AS $$
    DELETE FROM users WHERE id = $1;
  $$ LANGUAGE sql;

  CREATE OR REPLACE FUNCTION "getUsers"() RETURNS SETOF users AS $$
    SELECT * FROM users;
  $$ LANGUAGE sql;
  |]

userFuncsDrop :: PostgresM ()
userFuncsDrop = void $ pgExecute [sqlExp|
  DROP FUNCTION IF EXISTS "postUser"(u "userCreate");
  DROP FUNCTION IF EXISTS "getUsers"();
  DROP FUNCTION IF EXISTS "getUser"(int);
  DROP FUNCTION IF EXISTS "deleteUser"(int);
  DROP TYPE IF EXISTS "userRegister" CASCADE;
  DROP TABLE IF EXISTS "users";
  |]
