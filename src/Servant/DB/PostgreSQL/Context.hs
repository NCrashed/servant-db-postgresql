{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-|
Module      : Servant.DB.PostgreSQL.Context
Description : Query context that is used as temp storage
Portability : Not portable
-}
module Servant.DB.PostgreSQL.Context(
    Argument(..)
  , QueryArg(..)
  , QueryContext(..)
  , newQueryContext
  , addQueryArgument
  , queryStoredFunction
  ) where

import           Data.Foldable
import           Data.Kind
import           Data.List                        (intersperse)
import           Data.Monoid
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as S
import           Data.Text                        (Text)
import           Database.PostgreSQL.Query
import           Database.PostgreSQL.Simple.Types hiding (Default)
import           Servant.DB.PostgreSQL.Variadic
import           Servant.DB.PostgreSQL.Default
import GHC.Generics

-- | Captures special cases of stored function arguments
data Argument a =
    ArgVariadic (Variadic a) -- ^ Variadic argument has uncommon call syntax
  | ArgDefault (Default a) -- ^ Default keyword
  | ArgSimple a -- ^ Common case
  deriving (Generic, Eq, Show)

-- | Encapsulated argument that can be serialized into field or type checked
--
-- The type parameter can be 'ToField' for query generation or 'Typeable' for
-- type checking of DB signature.
data QueryArg (r :: * -> Constraint) = forall a . r a => QueryArg (Argument a)

-- | Check whether an argument is not specified
isDefaultArg :: QueryArg r -> Bool
isDefaultArg (QueryArg (ArgDefault Default)) = True
isDefaultArg _ = False

-- | Catches intermediate parameters for query
data QueryContext (r :: * -> Constraint) = QueryContext {
  -- | List of named and positional arguments
  queryArguments :: !(Seq (Maybe Text, QueryArg r))
  -- | Schema name
, querySchema    :: !(Maybe Text)
  -- | Whether the query returns void
, queryVoid      :: !Bool
}

-- | New empty query context
newQueryContext :: QueryContext r
newQueryContext = QueryContext {
    queryArguments = mempty
  , querySchema = Nothing
  , queryVoid = False
  }

-- | Add new argument to query context
addQueryArgument :: r a
  => Maybe Text -- ^ Name of argument, 'Nothing' for positional arguments
  -> Argument a -- ^ Value of argument
  -> QueryContext r -- ^ Context
  -> QueryContext r -- ^ New context with the argument
addQueryArgument name a ctx = ctx {
    queryArguments = queryArguments ctx S.|> (name, QueryArg a)
  }

-- | Helper to split positional and named arguments
--
-- PG call conventions require that named arguments cannot
-- precede positional ones.
querySplitArguments :: QueryContext r
  -> (Seq (QueryArg r), Seq (Text, QueryArg r))
querySplitArguments QueryContext{..} = foldl' go (mempty, mempty) queryArguments
  where
    go :: (Seq (QueryArg r), Seq (Text, QueryArg r)) -> (Maybe Text, QueryArg r)
      -> (Seq (QueryArg r), Seq (Text, QueryArg r))
    go (!posed, !named) (mn, a) = case mn of
      Nothing -> (posed S.|> a, named)
      Just n  -> (posed, named S.|> (n, a))

-- | Construct query that calls DB stored function
queryStoredFunction
  :: forall r . r ~ ToField
  => Text -- ^ Name of function
  -> QueryContext r -- ^ Context
  -> SqlBuilder
queryStoredFunction name ctx =
     "SELECT "
  <> (if queryVoid ctx then mempty else "* FROM ")
  <> toSqlBuilder (QualifiedIdentifier (querySchema ctx) name)
  <> "("
  <> (if S.null posed then mempty
      else posedBuilder <> (if S.null named then mempty else ", "))
  <> (if S.null named then mempty else namedBuilder)
  <> ")"
  <> (if queryVoid ctx then ";" else " as t;")
  where
    (posed', named') = querySplitArguments ctx
    posed = S.filter (not . isDefaultArg) posed'
    named = S.filter (not . isDefaultArg . snd) named'
    posedBuilder =  mconcat (addCommas $ argPosedBuilder <$> toList posed)
    namedBuilder = mconcat (addCommas $ uncurry argNamedBuilder <$> toList named)

    addCommas = intersperse ", "
    argPosedBuilder :: QueryArg r -> SqlBuilder
    argPosedBuilder (QueryArg marg) = case marg of
      ArgVariadic (Variadic va) -> "VARIADIC " <> mkValue va
      ArgDefault a -> case a of
        Default -> "" -- already handled
        Specific a -> mkValue a
      ArgSimple a -> mkValue a

    argNamedBuilder :: Text -> QueryArg r -> SqlBuilder
    argNamedBuilder aname (QueryArg marg) = case marg of
      ArgVariadic (Variadic a) -> "VARIADIC " <> toSqlBuilder (Identifier aname) <> " => " <> mkValue a
      ArgDefault a -> case a of
        Default -> "" -- already handled
        Specific a -> toSqlBuilder (Identifier aname) <> " => " <> mkValue a
      ArgSimple a -> toSqlBuilder (Identifier aname) <> " => " <> mkValue a
