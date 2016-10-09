{-# LANGUAGE  TypeOperators #-}
{-# LANGUAGE  DataKinds #-}
module Servant.PostgreSQLSpec(spec) where

import DB
import Fixture
import Test.Hspec hiding (Arg)
import Data.Proxy
import Test.HUnit

import Servant.API.DB
import Servant.DB.PostgreSQL

type SquareAPI = Arg "a" Int :> Procedure "square" Int

square :: Int -> PostgresM Int
square = deriveDB (Proxy :: Proxy SquareAPI) (Proxy :: Proxy PostgresM)

spec :: Spec
spec = before migrateFixture $ after (const deleteFixture) $
  describe "Servant.DB.PostgreSQL" $
    it "can call simple stored functions" $ do
      b <- runDB $ square 4
      assertEqual "4^2 = 16" 16 b
