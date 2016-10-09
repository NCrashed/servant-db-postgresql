{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-|
Module      : Servant.DB.PostgreSQL.HasDB
Description : Deriving DB client from API
Portability : Not portable
-}
module Servant.DB.PostgreSQL.HasDB(
    deriveDB
  , HasDB(..)
  ) where

import           Data.Monoid
import           Data.Proxy
import           Data.Text                     (pack)
import           Database.PostgreSQL.Query
import           GHC.TypeLits
import           Servant.API
import           Servant.API.DB
import           Servant.DB.PostgreSQL.Context

-- | Derive DB client from API
deriveDB :: HasDB layout m
  => Proxy layout -- ^ API layout
  -> Proxy m -- ^ PostgreSQL monad we operate in
  -> DB layout m -- ^ Derived functions
deriveDB layout m = deriveDBWithCtx layout m newQueryContext

-- | Derive DB client from API
class HasDB layout (m :: * -> *) where
  -- | Associated type of deriving result
  type DB layout m :: *

  -- | Derive DB client from API layout
  deriveDBWithCtx :: Proxy layout -> Proxy m -> QueryContext
    -> DB layout m

-- | Deriving several procedures to query DB API
--
-- @
-- type API = Procedure "time" Integer
--   :<|> Arg "a" Int :> Procedure "square" Int
--
-- data MyMonad m a -- Your application monad with connection pool and logger
-- instance HasPostgres m
-- instance MonadLogger m
--
-- time :: MyMonad Integer
-- square :: Int -> MyMonad Int
-- (time, square) = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
-- @
--
-- Upper example will derive separate endpoints with the following SQL calls:
-- >>> SELECT time();
-- >>> SELECT square("a" => ?);
instance (HasDB api1 m, HasDB api2 m) => HasDB (api1 :<|> api2) m where
  type DB (api1 :<|> api2) m = DB api1 m :<|> DB api2 m

  deriveDBWithCtx _ m ctx =
         deriveDBWithCtx (Proxy :: Proxy api1) m ctx
    :<|> deriveDBWithCtx (Proxy :: Proxy api2) m ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving several procedures to query DB API
--
-- @
-- type API = "public" :> Procedure "time" Integer
--
-- data MyMonad m a -- Your application monad with connection pool and logger
-- instance HasPostgres m
-- instance MonadLogger m
--
-- time :: MyMonad Integer
-- time = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
-- @
--
-- The `time` function will call DB with the:
-- >>> SELECT public.time();
--
-- Note that there could be only one schema marker. If there are more than one
-- the later (righter) will override previous ones.
instance (KnownSymbol n, HasDB api m) => HasDB (n :> api) m where
  type DB (n :> api) m = DB api m

  deriveDBWithCtx _ m ctx = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      ctx' = ctx { querySchema = Just n }
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with arguments
--
-- @
-- type API = Arg "a" Int :> Arg "b" Int :> Procedure "sum" Int
--
-- data MyMonad m a -- Your application monad with connection pool and logger
-- instance HasPostgres m
-- instance MonadLogger m
--
-- dbSum :: Int -> Int -> MyMonad Int
-- dbSum = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
-- @
--
-- Upper example will derive the following SQL call:
-- >>> SELECT sum("a" => ?, "b" => ?);
instance (KnownSymbol n, ToField a, HasDB api m) => HasDB (Arg n a :> api) m where
  type DB (Arg n a :> api) m = a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      ctx' = addQueryArgument n a ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with multiple result
--
-- @
-- data User -- user data
-- instance FromRow User
--
-- type API = Procedure "users" User
--
-- data MyMonad m a -- Your application monad with connection pool and logger
-- instance HasPostgres m
-- instance MonadLogger m
--
-- getUsers :: MyMonad [Users]
-- getUsers = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
-- @
--
-- Upper example will derive the following SQL call:
-- >>> SELECT users();
--
-- And the instance expects that `users` function return type is `SETOF user`.
instance {-# OVERLAPPING #-} (KnownSymbol n, FromRow a, MonadPostgres m)
  => HasDB (Procedure n [a]) m where
  type DB (Procedure n [a]) m = m [a]

  deriveDBWithCtx _ _ ctx = pgQuery q
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      q = queryStoredFunction n ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with single result
--
-- @
-- type API = Arg "a" Int -> Procedure "square" Int
--
-- data MyMonad m a -- Your application monad with connection pool and logger
-- instance HasPostgres m
-- instance MonadLogger m
--
-- square :: Int -> MyMonad Int
-- square = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
-- @
--
-- Upper example will derive the following SQL call:
-- >>> SELECT square();
--
-- The instance expects that return type of SQL stored function is single value.
instance {-# OVERLAPPABLE #-} (KnownSymbol n, FromField a, MonadPostgres m)
  => HasDB (Procedure n a) m where
  type DB (Procedure n a) m = m a

  deriveDBWithCtx _ _ ctx = do
    mv <- pgQuery q
    case mv of
      [] -> fail $ "deriveDBWithCtx: received zero results when expected"
        <> " exactly one. PG Function: " <> n'
      (Only v : _) -> return v
    where
      n' = symbolVal (Proxy :: Proxy n)
      n = pack n'
      q = queryStoredFunction n ctx
  {-# INLINE deriveDBWithCtx #-}
