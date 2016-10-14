{-|
Module      : Servant.DB.PostgreSQL
Description : Reexport tools for deriving of PostgreSQL client for DB API.
Portability : Not portable

= Quick start

Automatic derive of [typed DB API](https://github.com/NCrashed/servant-db) based
on [postgresql-query](http://hackage.haskell.org/package/postgresql-query) package.

How to use:

* Define your instances of `MonadLogger` and `HasPostgres` for your app
monad:

> newtype PostgresM a = PostgresM { runPostgresM :: PgMonadT (LoggingT IO) a }
>  deriving (Functor, HasPostgres, MonadLogger, Monad, Applicative, MonadBase IO)

* Define type level API:

> type UserId = Int
>
> data RegisterUser = RegisterUser {
>   userRegName     :: String
> , userRegPassword :: String
> , userRegRegTime  :: Day
> } deriving (Eq)
>
> deriveToRow ''RegisterUser
>
> data User = User {
>   userId       :: UserId
> , userName     :: String
> , userPassword :: String
> , userRegTime  :: Day
> } deriving (Eq)
>
> deriveFromRow ''User
> deriveToRow ''User
>
> type UserAPI =
>        ArgNamed "u" (Composite RegisterUser)
>     :> Procedure "postUser" (Only Int)
>   :<|> ArgPos Int
>     :> Procedure "getUser" (Maybe User)
>   :<|> ArgPos Int
>     :> Procedure "deleteUser" ()
>   :<|> Procedure "getUsers" [User]

* Derive client functions from the API:

> postUser :: Composite RegisterUser -> PostgresM (Only Int)
> getUser :: Int -> PostgresM (Maybe User)
> deleteUser :: Int -> PostgresM ()
> getUsers :: PostgresM [User]
> (      postUser
>   :<|> getUser
>   :<|> deleteUser
>   :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)

= Features

* Call functions in schema with `:>` operator:

> type API = "test" :> ArgPos Int :> Procedure "square" (Only Int)

That will call function `test.square(int)`.

* Composite types are defined with `Composite a` wrapper:

> type API = ArgNamed "u" (Composite UserCreate) :> Procedure "postUser" (Only Int)

* Support for arrays:

> type API = ArgPos (PGArray Int) :> Procedure "mleast" (Maybe (Only Int))

* Support for variadic arguments:

> type API = ArgPos (Variadic Int) :> Procedure "mleast" (Maybe (Only Int))

* Support for default arguments with `Default` wrapper:

> type API = ArgPos (Default Int) :> Procedure "foo" (Only Int)

-}
module Servant.DB.PostgreSQL(
  -- * Support for composite types
    module Servant.DB.PostgreSQL.Composite
  -- * Support for variadic types
  , module Servant.DB.PostgreSQL.Variadic
  -- * Deriving of DB clients
  , module Servant.DB.PostgreSQL.HasDB
  ) where

import           Servant.DB.PostgreSQL.Composite
import           Servant.DB.PostgreSQL.HasDB
import           Servant.DB.PostgreSQL.Variadic
