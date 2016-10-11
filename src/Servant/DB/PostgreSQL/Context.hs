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
    QueryArg(..)
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
import           Database.PostgreSQL.Simple.Types

-- | Encapsulated argument that can be serialized into field or type checked
--
-- The type parameter can be 'ToField' for query generation or 'Typeable' for
-- type checking of DB signature.
data QueryArg (r :: * -> Constraint) = forall a . r a => QueryArg a

instance ToField ~ r => ToField (QueryArg r) where
  toField (QueryArg a) = toField a

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
  -> a -- ^ Value of argument
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
    (posed, named) = querySplitArguments ctx
    posedBuilder =  mconcat (addCommas $ argPosedBuilder <$> toList posed)
    namedBuilder = mconcat (addCommas $ uncurry argNamedBuilder <$> toList named)

    addCommas = intersperse ", "
    argPosedBuilder :: QueryArg r -> SqlBuilder
    argPosedBuilder (QueryArg a) = mkValue a
    argNamedBuilder :: Text -> QueryArg r -> SqlBuilder
    argNamedBuilder aname (QueryArg a) = toSqlBuilder (Identifier aname)
      <> " => " <> mkValue a
