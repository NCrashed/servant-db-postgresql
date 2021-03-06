{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-|
Module      : Servant.DB.PostgreSQL.HasDB
Description : Deriving DB client from API
Portability : Not portable
-}
module Servant.DB.PostgreSQL.HasDB(
    deriveDB
  , HasDB(..)
  , module Reexport
  ) where

import           Control.Applicative
import           Control.Monad                      (replicateM_)
import           Data.Monoid
import           Data.Proxy
import           Data.Text                          (pack)
import           Database.PostgreSQL.Query
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types   (Null)
import           GHC.TypeLits
import           Servant.API
import           Servant.API.DB
import           Servant.DB.PostgreSQL.Context
import           Servant.DB.PostgreSQL.Default
import           Servant.DB.PostgreSQL.Variadic

import           Database.PostgreSQL.Simple         as Reexport (Only (..))

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
  deriveDBWithCtx :: Proxy layout -> Proxy m -> QueryContext ToField
    -> DB layout m

-- | Deriving several procedures to query DB API
--
-- > type API = Procedure "time" Integer
-- >   :<|> ArgNamed "a" Int :> Procedure "square" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > time :: MyMonad (Only Integer)
-- > square :: Int -> MyMonad (Only Int)
-- > (time, square) = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive separate endpoints with the following SQL calls:
--
-- >>> SELECT time();
--
-- >>> SELECT square("a" => ?);
instance (HasDB api1 m, HasDB api2 m) => HasDB (api1 :<|> api2) m where
  type DB (api1 :<|> api2) m = DB api1 m :<|> DB api2 m

  deriveDBWithCtx _ m ctx =
         deriveDBWithCtx (Proxy :: Proxy api1) m ctx
    :<|> deriveDBWithCtx (Proxy :: Proxy api2) m ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving several procedures to query DB API
--
-- > type API = "public" :> Procedure "time" (Only Integer)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > time :: MyMonad (Only Integer)
-- > time = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- The `time` function will call DB with the:
--
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

-- | Deriving call to DB procedure with named arguments
--
-- > type API = ArgNamed "a" Int :> ArgNamed "b" Int :> Procedure "sum" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbSum :: Int -> Int -> MyMonad (Only Int)
-- > dbSum = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM sum("a" => ?, "b" => ?) AS t;
instance {-# OVERLAPPABLE #-} (KnownSymbol n, ToField a, HasDB api m) => HasDB (ArgNamed n a :> api) m where
  type DB (ArgNamed n a :> api) m = a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      ctx' = addQueryArgument (Just n) (ArgSimple a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with named variadic arguments
--
-- > type API = ArgNamed "arr" (Variadic Int) :> Procedure "mleast" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbMleast :: Variadic Int -> MyMonad (Only Int)
-- > dbMleast = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM mleast(VARIADIC "arr" => ?) AS t;
instance {-# OVERLAPPING #-} (KnownSymbol n, ToField a, HasDB api m) => HasDB (ArgNamed n (Variadic a) :> api) m where
  type DB (ArgNamed n (Variadic a) :> api) m = Variadic a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      ctx' = addQueryArgument (Just n) (ArgVariadic a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with named default arguments
--
-- > type API = ArgNamed "a" (Defaultable Int) :> Procedure "foo" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbFoo :: Defaultable Int -> MyMonad (Only Int)
-- > dbFoo = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM default(DEFAULT) AS t;
instance {-# OVERLAPPING #-} (KnownSymbol n, ToField a, HasDB api m) => HasDB (ArgNamed n (Default a) :> api) m where
  type DB (ArgNamed n (Default a) :> api) m = Default a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      ctx' = addQueryArgument (Just n) (ArgDefault $ unDefault a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with positional arguments
--
-- > type API = Arg Int :> Arg Int :> Procedure "sum" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbSum :: Int -> Int -> MyMonad (Only Int)
-- > dbSum = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM sum(?, ?) AS t;
instance {-# OVERLAPPABLE #-} (ToField a, HasDB api m) => HasDB (ArgPos a :> api) m where
  type DB (ArgPos a :> api) m = a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      ctx' = addQueryArgument Nothing (ArgSimple a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with positional variadic arguments
--
-- > type API = ArgPos (Variadic Int) :> Procedure "mleast" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbMleast :: Variadic Int -> MyMonad (Only Int)
-- > dbMleast = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM mleast(VARIADIC ?) AS t;
instance {-# OVERLAPPING #-} (ToField a, HasDB api m) => HasDB (ArgPos (Variadic a) :> api) m where
  type DB (ArgPos (Variadic a) :> api) m = Variadic a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      ctx' = addQueryArgument Nothing (ArgVariadic a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with positional default arguments
--
-- > type API = ArgPos (Default Int) :> Procedure "foo" (Only Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > dbFoo :: Default Int -> MyMonad (Only Int)
-- > dbFoo = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM foo(DEFAULT) AS t;
instance {-# OVERLAPPING #-} (ToField a, HasDB api m) => HasDB (ArgPos (Default a) :> api) m where
  type DB (ArgPos (Default a) :> api) m = Default a -> DB api m

  deriveDBWithCtx _ m ctx a = deriveDBWithCtx (Proxy :: Proxy api) m ctx'
    where
      ctx' = addQueryArgument Nothing (ArgDefault $ unDefault a) ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with no return type
--
-- > data User -- user data
-- > instance ToRow User
-- >
-- > type API = Arg "user" User :> Procedure "registerUser" ()
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > getUsers :: User -> MyMonad ()
-- > getUsers = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT registerUser("user" => ?);
--
-- And the instance expects that `users` function return type is `SETOF user`.
instance {-# OVERLAPPING #-} (KnownSymbol n, MonadPostgres m)
  => HasDB (Procedure n ()) m where
  type DB (Procedure n ()) m = m ()

  deriveDBWithCtx _ _ ctx = do
    (_ :: [Only ()]) <- pgQuery q
    return ()
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      q = queryStoredFunction n ctx { queryVoid = True }
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with multiple result
--
-- > data User -- user data
-- > instance FromRow User
-- >
-- > type API = Procedure "users" [User]
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > getUsers :: MyMonad [User]
-- > getUsers = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM users() AS t;
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

nullRow :: RowParser Null
nullRow = field

instance {-# OVERLAPPABLE #-} FromRow a => FromRow (Maybe a) where
  fromRow = do
    n <- numFieldsRemaining
    (replicateM_ n nullRow *> pure Nothing) <|> (Just <$> fromRow)

-- | Deriving call to DB procedure with optional result
--
-- > data User -- user data
-- > instance FromRow User
-- >
-- > type API = ArgPos Int :> Procedure "user" (Maybe User)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > getUsers :: MyMonad (Maybe User)
-- > getUsers = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM user(?) AS t;
instance {-# OVERLAPPING #-} (KnownSymbol n, FromRow a, MonadPostgres m)
  => HasDB (Procedure n (Maybe a)) m where
  type DB (Procedure n (Maybe a)) m = m (Maybe a)

  deriveDBWithCtx _ _ ctx = do
    res <- pgQuery q
    case res of
      x : _ -> return x
      _     -> return Nothing
    where
      n = pack $ symbolVal (Proxy :: Proxy n)
      q = queryStoredFunction n ctx
  {-# INLINE deriveDBWithCtx #-}

-- | Deriving call to DB procedure with single result
--
-- > type API = Arg "a" Int -> Procedure "squareReturning" (Int, Int)
-- >
-- > data MyMonad m a -- Your application monad with connection pool and logger
-- > instance HasPostgres m
-- > instance MonadLogger m
-- >
-- > square :: Int -> MyMonad (Int, Int)
-- > square = deriveDB (Proxy :: Proxy API) (Proxy :: Proxy MyMonad)
--
-- Upper example will derive the following SQL call:
--
-- >>> SELECT * FROM squareReturning() AS t;
--
-- The instance expects that return type of SQL stored function is a single row.
instance {-# OVERLAPPABLE #-} (KnownSymbol n, FromRow a, MonadPostgres m)
  => HasDB (Procedure n a) m where
  type DB (Procedure n a) m = m a

  deriveDBWithCtx _ _ ctx = do
    mv <- pgQuery q
    case mv of
      [] -> fail $ "deriveDBWithCtx: received zero results when expected"
        <> " exactly one. PG Function: " <> n'
      (v : _) -> return v
    where
      n' = symbolVal (Proxy :: Proxy n){--}
      n = pack n'
      q = queryStoredFunction n ctx
  {-# INLINE deriveDBWithCtx #-}
