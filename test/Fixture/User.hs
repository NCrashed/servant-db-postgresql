{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Fixture.User(
    User(..)
  , UserCreate(..)
  , userToUserCreate
  , userCreateToUser
  ) where

import           Data.DeriveTH
import           Data.Text
import           Data.Time
import           Database.PostgreSQL.Query
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances

-- | Sample User record that is used to test custom types as return type
data User = User {
  userId       :: !Int
, userName     :: !String
, userPassword :: !String
, userRegDay   :: !Day
} deriving (Generic, Eq, Show)

derive makeArbitrary ''User
deriveFromRow ''User
deriveToRow ''User

-- | Sample User record that is used to test custom types as return type
data UserCreate = UserCreate {
  userCreateName     :: !String
, userCreatePassword :: !String
, userCreateRegDay  :: !Day
} deriving (Generic, Eq, Show)

derive makeArbitrary ''UserCreate
deriveFromRow ''UserCreate
deriveToRow ''UserCreate

userToUserCreate :: User -> UserCreate
userToUserCreate User{..} = UserCreate {
    userCreateName = userName
  , userCreatePassword = userPassword
  , userCreateRegDay = userRegDay
  }

userCreateToUser :: UserCreate -> Int -> User
userCreateToUser UserCreate{..} i = User {
    userId = i
  , userName = userCreateName
  , userPassword = userCreatePassword
  , userRegDay = userCreateRegDay
  }
