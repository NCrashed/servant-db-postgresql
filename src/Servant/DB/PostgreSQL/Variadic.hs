{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Servant.DB.PostgreSQL.Variadic(
    Variadic(..)
  , PGArray(..)
  ) where

import           Database.PostgreSQL.Simple.Types

newtype Variadic a = Variadic { unVariadic :: PGArray a }
  deriving (Functor, Eq, Ord, Read, Show)
