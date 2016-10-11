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

square :: Int -> PostgresM (Only Int)
square = deriveDB (Proxy :: Proxy SquareAPI) (Proxy :: Proxy PostgresM)

squareSchema :: Int -> PostgresM (Only Int)
squareSchema = deriveDB (Proxy :: Proxy SquareSchemaAPI) (Proxy :: Proxy PostgresM)

succAndPred :: Int -> PostgresM (Int, Int)
succAndPred = deriveDB (Proxy :: Proxy SuccAndPredAPI) (Proxy :: Proxy PostgresM)

postUser :: Composite UserCreate -> PostgresM (Only Int)
getUsers :: PostgresM [User]
(postUser :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)

ordered1 :: Int -> Int -> Int -> PostgresM (Only Int)
ordered1 = deriveDB (Proxy :: Proxy OrderedAPI1) (Proxy :: Proxy PostgresM)

ordered2 :: Int -> Int -> Int -> PostgresM (Only Int)
ordered2 = deriveDB (Proxy :: Proxy OrderedAPI2) (Proxy :: Proxy PostgresM)

voidProc :: PostgresM ()
voidProc = deriveDB (Proxy :: Proxy VoidAPI) (Proxy :: Proxy PostgresM)

spec :: Spec
spec = describe "Servant.DB.PostgreSQL" $ do
  it "can call simple stored functions" $ withSquareFunc $ do
    Only b <- runDB $ square 4
    assertEqual "4^2 = 16" 16 b
  it "can call stored functions in schema" $ withTestSchema $ withSquareSchema $ do
    Only b <- runDB $ squareSchema 4
    assertEqual "4^2 = 16" 16 b
  it "supports row return" $ withSuccAndPred $ do
    b <- runDB $ succAndPred 4
    assertEqual "4+1 and 4-1 = (5, 3)" (5,3) b
  it "supports table return" $ withUserFuncs $ do
    user <- generate arbitrary
    Only i <- runDB $ postUser $ Composite user
    users <- runDB getUsers
    assertEqual "wrote == read" [userCreateToUser user i] users
  it "handles ordered arguments" $ withOrderedFuncs $ do
    Only b <- runDB $ ordered1 1 2 3
    assertEqual "1+2*2+3*3 = 14" 14 b
  it "handles mixed argument style" $ withOrderedFuncs $ do
    Only b <- runDB $ ordered2 1 2 3
    assertEqual "1+3*2+2*3 = 13" 13 b
  it "handles void return type" $ withVoid $ runDB voidProc


