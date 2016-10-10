{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Servant.PostgreSQLSpec(spec) where

import           Data.Proxy
import           Data.Text             (Text)
import           Data.Time
import           DB
import           Fixture
import           GHC.Generics
import           Test.Hspec            hiding (Arg)
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API.DB
import           Servant.DB.PostgreSQL

type SquareAPI = ArgNamed "a" Int :> Procedure "square1" (Only Int)
type SquareSchemaAPI = "test" :> ArgNamed "b" Int :> Procedure "square2" (Only Int)
type SuccAndPredAPI = ArgNamed "n" Int :> Procedure "succAndPred" (Int, Int)
type UserAPI =
       ArgNamed "u" (Composite UserCreate) :> Procedure "postUser" (Only Int)
  :<|> Procedure "getUsers" [User]

square :: Int -> PostgresM (Only Int)
square = deriveDB (Proxy :: Proxy SquareAPI) (Proxy :: Proxy PostgresM)

squareSchema :: Int -> PostgresM (Only Int)
squareSchema = deriveDB (Proxy :: Proxy SquareSchemaAPI) (Proxy :: Proxy PostgresM)

succAndPred :: Int -> PostgresM (Int, Int)
succAndPred = deriveDB (Proxy :: Proxy SuccAndPredAPI) (Proxy :: Proxy PostgresM)

postUser :: Composite UserCreate -> PostgresM (Only Int)
getUsers :: PostgresM [User]
(postUser :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)

spec :: Spec
spec = before migrateFixture $ after (const deleteFixture) $
  describe "Servant.DB.PostgreSQL" $ do
    it "can call simple stored functions" $ do
      Only b <- runDB $ square 4
      assertEqual "4^2 = 16" 16 b
    it "can call stored functions in schema" $ do
      Only b <- runDB $ squareSchema 4
      assertEqual "4^2 = 16" 16 b
    it "supports row return" $ do
      b <- runDB $ succAndPred 4
      assertEqual "4+1 and 4-1 = (5, 3)" (5,3) b
    it "supports table return" $ do
      user <- generate arbitrary
      Only i <- runDB $ postUser $ Composite user
      users <- runDB getUsers
      assertEqual "wrote == read" [userCreateToUser user i] users
