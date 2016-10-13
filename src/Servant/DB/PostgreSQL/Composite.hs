{-|
Module      : Servant.DB.PostgreSQL.Composite
Description : Composite types support.
Portability : Not portable
-}
module Servant.DB.PostgreSQL.Composite(
    Composite(..)
  ) where

import           Data.List                          (intersperse)
import           Data.Typeable
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

-- | Wrapper around 'a' that indicates that the type can be used as composite
-- type.
--
-- >>> type UserAPI = Arg "u" (Composite User) :> Procedure "insertUser" ()
newtype Composite a = Composite { unComposite :: a }
  deriving (Typeable, Eq, Generic, Show, Read, Ord)

instance ToRow a => ToField (Composite a) where
  toField (Composite a) = Many [
      Plain "("
    , Many $ intersperse (Plain ",") $ toRow a
    , Plain ")"
    ]
