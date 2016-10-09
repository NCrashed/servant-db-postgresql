{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Servant.PostgreSQLSpec(spec) where

import           Data.Proxy
import           DB
import           Fixture
import           Test.Hspec            hiding (Arg)
import           Test.HUnit

import           Servant.API.DB
import           Servant.DB.PostgreSQL

type SquareAPI = Arg "a" Int :> Procedure "square1" (Only Int)
type SquareSchemaAPI = "test" :> Arg "b" Int :> Procedure "square2" (Only Int)
type SuccAndPredAPI = Arg "n" Int :> Procedure "succAndPred" (Int, Int)

square :: Int -> PostgresM (Only Int)
square = deriveDB (Proxy :: Proxy SquareAPI) (Proxy :: Proxy PostgresM)

squareSchema :: Int -> PostgresM (Only Int)
squareSchema = deriveDB (Proxy :: Proxy SquareSchemaAPI) (Proxy :: Proxy PostgresM)

succAndPred :: Int -> PostgresM (Int, Int)
succAndPred = deriveDB (Proxy :: Proxy SuccAndPredAPI) (Proxy :: Proxy PostgresM)

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
