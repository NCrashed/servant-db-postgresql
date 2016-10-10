{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fixture.User(
    User(..)
  , UserCreate(..)
  , userToUserCreate
  , userCreateToUser
  ) where

import           Data.DeriveTH
import           Data.Time
import           Database.PostgreSQL.Query
import           GHC.Generics
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances

newtype NotNullString = NotNullString String
  deriving (Eq, Show, FromField, ToField)

instance Arbitrary NotNullString where
  arbitrary = NotNullString . filter (/= '\0') <$> arbitrary

-- | Sample User record that is used to test custom types as return type
data User = User {
  userId       :: !Int
, userName     :: !NotNullString
, userPassword :: !NotNullString
, userRegDay   :: !Day
} deriving (Generic, Eq, Show)

derive makeArbitrary ''User
deriveFromRow ''User
deriveToRow ''User

-- | Sample User record that is used to test custom types as return type
data UserCreate = UserCreate {
  userCreateName     :: !NotNullString
, userCreatePassword :: !NotNullString
, userCreateRegDay   :: !Day
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
