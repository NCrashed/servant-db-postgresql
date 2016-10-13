module Servant.PostgreSQLSpec(spec) where

import           Data.Proxy
import           DB
import           Fixture
import           Test.Hspec            hiding (Arg)
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API.DB
import           Servant.API.DB.Default
import           Servant.DB.PostgreSQL

square :: Int -> PostgresM (Only Int)
square = deriveDB (Proxy :: Proxy SquareAPI) (Proxy :: Proxy PostgresM)

squareSchema :: Int -> PostgresM (Only Int)
squareSchema = deriveDB (Proxy :: Proxy SquareSchemaAPI) (Proxy :: Proxy PostgresM)

succAndPred :: Int -> PostgresM (Int, Int)
succAndPred = deriveDB (Proxy :: Proxy SuccAndPredAPI) (Proxy :: Proxy PostgresM)

postUser :: Composite UserCreate -> PostgresM (Only Int)
getUser :: Int -> PostgresM (Maybe User)
deleteUser :: Int -> PostgresM ()
getUsers :: PostgresM [User]
(      postUser
  :<|> getUser
  :<|> deleteUser
  :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)

ordered1 :: Int -> Int -> Int -> PostgresM (Only Int)
ordered1 = deriveDB (Proxy :: Proxy OrderedAPI1) (Proxy :: Proxy PostgresM)

ordered2 :: Int -> Int -> Int -> PostgresM (Only Int)
ordered2 = deriveDB (Proxy :: Proxy OrderedAPI2) (Proxy :: Proxy PostgresM)

voidProc :: PostgresM ()
voidProc = deriveDB (Proxy :: Proxy VoidAPI) (Proxy :: Proxy PostgresM)

variadicProc :: Variadic Int -> PostgresM (Maybe (Only Int))
variadicProc = deriveDB (Proxy :: Proxy VariadicAPI) (Proxy :: Proxy PostgresM)

arrayProc :: PGArray Int -> PostgresM (Maybe (Only Int))
arrayProc = deriveDB (Proxy :: Proxy ArrayAPI) (Proxy :: Proxy PostgresM)

defaultProc :: Default Int -> Default Int -> PostgresM (Only Int)
defaultProc = deriveDB (Proxy :: Proxy DefaultAPI) (Proxy :: Proxy PostgresM)

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
    let user' = userCreateToUser user i
    assertEqual "wrote == read list" [user'] users
    muser1 <- runDB $ getUser i
    assertEqual "wrote == read single" (Just user') muser1
    runDB $ deleteUser i
    muser2 <- runDB $ getUser i
    assertEqual "deleted cannot be read" Nothing muser2
  it "handles ordered arguments" $ withOrderedFuncs $ do
    Only b <- runDB $ ordered1 1 2 3
    assertEqual "1+2*2+3*3 = 14" 14 b
  it "handles mixed argument style" $ withOrderedFuncs $ do
    Only b <- runDB $ ordered2 1 2 3
    assertEqual "1+3*2+2*3 = 13" 13 b
  it "handles void return type" $ withVoid $ runDB voidProc
  it "handles variadic type" $ withVariadic $ do
    res1 <- runDB $ variadicProc (Variadic $ PGArray [10, -1, 5, 4])
    assertEqual "mleast [10, -1, 5, 4] = -1" (Just $ Only (-1)) res1
    res2 <- runDB $ variadicProc (Variadic $ PGArray [])
    assertEqual "mleast [] = NULL" Nothing res2
  it "handles array type" $ withArrayFuncs $ do
    res1 <- runDB $ arrayProc (PGArray [10, -1, 5, 4])
    assertEqual "mleast [10, -1, 5, 4] = -1" (Just $ Only (-1)) res1
    res2 <- runDB $ arrayProc (PGArray [])
    assertEqual "mleast [] = NULL" Nothing res2
  it "handles default values" $ withDefaultFuncs $ do
    Only res1 <- runDB $ defaultProc (Specific 1) (Specific 1)
    assertEqual "1+1 = 2" 2 res1
    Only res2 <- runDB $ defaultProc (Specific 1) Default
    assertEqual "1+0 = 1" 1 res2
    Only res3 <- runDB $ defaultProc Default (Specific 2)
    assertEqual "0+2 = 2" 2 res3
    Only res4 <- runDB $ defaultProc Default Default
    assertEqual "0+0 = 0" 0 res4


