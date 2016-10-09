{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
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

import           Data.Monoid
import           Data.Text                        (Text)
import           Database.PostgreSQL.Query
import           Database.PostgreSQL.Simple.Types

-- | Encapsulated argument that can be serialized into field
data QueryArg = forall a . ToField a => QueryArg a

instance ToField QueryArg where
  toField (QueryArg a) = toField a

-- | Catches intermediate parameters for query
data QueryContext = QueryContext {
  -- | List of named arguments
  queryArguments :: [(Text, QueryArg)]
  -- | Schema name
, querySchema    :: Maybe Text
}

-- | New empty query context
newQueryContext :: QueryContext
newQueryContext = QueryContext {
    queryArguments = []
  , querySchema = Nothing
  }

-- | Add new argument to query context
addQueryArgument :: ToField a
  => Text -- ^ Name of argument
  -> a -- ^ Value of argument
  -> QueryContext -- ^ Context
  -> QueryContext -- ^ New context with the argument
addQueryArgument name a ctx = ctx {
    queryArguments = (name, QueryArg a) : queryArguments ctx
  }

-- | Construct query that calls DB stored function
queryStoredFunction :: Text -- ^ Name of function
  -> QueryContext -- ^ Context
  -> SqlBuilder
queryStoredFunction name ctx = "SELECT * FROM"
  <> toSqlBuilder (QualifiedIdentifier (querySchema ctx) name)
  <> "("
  <> mconcat (uncurry argBuilder <$> queryArguments ctx)
  <> ") as t;"
  where
    argBuilder aname (QueryArg a) = toSqlBuilder (Identifier aname)
      <> " => " <> mkValue a
