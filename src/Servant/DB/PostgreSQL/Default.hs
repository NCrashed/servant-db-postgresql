{-# LANGUAGE DeriveFunctor #-}
{-|
Module      : Servant.DB.PostgreSQL.Default
Description : Default placeholder for stored functions
Portability : Not portable

Provides helper for default value of stored function argument.

The module can clash with 'Database.PostgreSQL.Simple.Types', so you can hide
conlicting 'Default' constructor:

@
import Database.PostgreSQL.Simple.Types hiding (Default)
@

-}
module Servant.DB.PostgreSQL.Default(
    Default(..)
  , toDefault
  , fromDefault
  , isSpecific
  , isDefault
  ) where

import qualified Data.ByteString.Char8              as BS
import           Data.List                          (intersperse)
import           Data.Typeable
import           GHC.Generics

-- | 'Maybe' like type that catches idea of arguments that can be omitted while
-- calling stored function.
--
-- >>> type UserAPI = Arg "phone" (Default String) :> Procedure "insertUser" ()
data Default a = Default | Specific a
  deriving (Typeable, Eq, Generic, Show, Read, Ord, Functor)

instance Applicative Default where
  pure = Specific
  (Specific f) <*> (Specific a) = Specific (f a)
  _ <*> _ = Default

instance Monad Default where
  return = pure
  Default >>= f = Default
  (Specific a) >>= f = f a

-- | Convert from 'Maybe'
toDefault :: Maybe a -> Default a
toDefault Nothing = Default
toDefault (Just a) = Specific a

-- | Convert to 'Maybe'
fromDefault :: Default a -> Maybe a
fromDefault Default = Nothing
fromDefault (Specific a) = Just a

-- | Is value of defaultable argument specified?
isSpecific :: Default a -> Bool
isSpecific (Specific _) = True
isSpecific _ = False

-- | Is value of defaultable argument specified?
isDefault :: Default a -> Bool
isDefault Default = True
isDefault _ = False
