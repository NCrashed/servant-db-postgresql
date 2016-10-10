{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DB(
    PostgresM
  , runDB
  ) where

import           Control.Monad.Base
import           Control.Monad.Logger
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Maybe
import           Database.PostgreSQL.Query
import           Options.Applicative

newtype PostgresM a = PostgresM { runPostgresM :: PgMonadT (LoggingT IO) a }
  deriving (Functor, HasPostgres, MonadLogger, Monad, Applicative, MonadBase IO)

dbConnection :: ByteString
dbConnection = "host=localhost dbname=servant_db_postgresql_test port=5432"

cliConnection :: IO (Maybe ByteString)
cliConnection = execParser opts
  where
    opts = info (helper <*> cliParser)
       (  fullDesc
       <> progDesc "Test suite for servant-db-postgresql package"
       <> header "Runs integration tests for servant-db-postgresql package" )

cliParser :: Parser (Maybe ByteString)
cliParser = optional $ BS.pack
  <$> strOption (
       long "connection"
    <> help "Custom connection string to PostgreSQL"
    )

 -- | Run tests with DB connection
runDB :: PostgresM a -> IO a
runDB m = do
  mconf <- cliConnection
  let conf = fromMaybe dbConnection mconf
  con <- connectPostgreSQL conf
  runStderrLoggingT . runPgMonadT con . runPostgresM $ m
