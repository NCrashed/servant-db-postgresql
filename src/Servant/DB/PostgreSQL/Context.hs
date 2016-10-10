{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
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
import           Data.List                        (intersperse)
import           Data.Monoid
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as S
import           Data.Text                        (Text)
import           Database.PostgreSQL.Query
import           Database.PostgreSQL.Simple.Types

-- | Encapsulated argument that can be serialized into field
data QueryArg = forall a . ToField a => QueryArg a

instance ToField QueryArg where
  toField (QueryArg a) = toField a

-- | Catches intermediate parameters for query
data QueryContext = QueryContext {
  -- | List of named and positional arguments
  queryArguments :: !(Seq (Maybe Text, QueryArg))
  -- | Schema name
, querySchema    :: !(Maybe Text)
}

-- | New empty query context
newQueryContext :: QueryContext
newQueryContext = QueryContext {
    queryArguments = mempty
  , querySchema = Nothing
  }

-- | Add new argument to query context
addQueryArgument :: ToField a
  => Maybe Text -- ^ Name of argument, 'Nothing' for positional arguments
  -> a -- ^ Value of argument
  -> QueryContext -- ^ Context
  -> QueryContext -- ^ New context with the argument
addQueryArgument name a ctx = ctx {
    queryArguments = queryArguments ctx S.|> (name, QueryArg a)
  }

-- | Helper to split positional and named arguments
--
-- PG call conventions require that named arguments cannot
-- precede positional ones.
querySplitArguments :: QueryContext
  -> (Seq QueryArg, Seq (Text, QueryArg))
querySplitArguments QueryContext{..} = foldl' go (mempty, mempty) queryArguments
  where
    go :: (Seq QueryArg, Seq (Text, QueryArg)) -> (Maybe Text, QueryArg)
      -> (Seq QueryArg, Seq (Text, QueryArg))
    go (!posed, !named) (mn, a) = case mn of
      Nothing -> (posed S.|> a, named)
      Just n  -> (posed, named S.|> (n, a))

-- | Construct query that calls DB stored function
queryStoredFunction :: Text -- ^ Name of function
  -> QueryContext -- ^ Context
  -> SqlBuilder
queryStoredFunction name ctx = "SELECT * FROM"
  <> toSqlBuilder (QualifiedIdentifier (querySchema ctx) name)
  <> "("
  <> (if S.null posed then mempty else posedBuilder)
  <> (if S.null named then mempty else namedBuilder)
  <> ") as t;"
  where
    (posed, named) = querySplitArguments ctx
    posedBuilder =  mconcat (addCommas $ argPosedBuilder <$> toList posed)
    namedBuilder = mconcat (addCommas $ uncurry argNamedBuilder <$> toList named)

    addCommas = intersperse ", "
    argPosedBuilder (QueryArg a) = mkValue a
    argNamedBuilder aname (QueryArg a) = toSqlBuilder (Identifier aname)
      <> " => " <> mkValue a
