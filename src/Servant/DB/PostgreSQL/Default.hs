{-|
  Module      : Servant.DB.PostgreSQL.Default
  Description : Newtype wrapper for marking an default argument
  Portability : Portable
-}
module Servant.DB.PostgreSQL.Default(
    Default(..)
  ) where

import           Data.Typeable
import           GHC.Generics

-- | Wrapper around 'Maybe' to distinguish default arguments from nullable ones
newtype Default a = Default { unDefault :: Maybe a }
  deriving (Eq, Ord, Show, Read, Generic, Typeable)
