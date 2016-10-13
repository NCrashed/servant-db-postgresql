servant-db-postgresql
=====================

Automatic derive of [typed DB API](https://github.com/NCrashed/servant-db) based 
on [postgresql-query](http://hackage.haskell.org/package/postgresql-query) package.

How to use:

1. Define your instances of `MonadLogger` and `HasPostgres` for your app
monad:
``` haskell
newtype PostgresM a = PostgresM { runPostgresM :: PgMonadT (LoggingT IO) a }
  deriving (Functor, HasPostgres, MonadLogger, Monad, Applicative, MonadBase IO)
```

2. Define type level API:
``` haskell
type UserId = Int

data RegisterUser = RegisterUser {
  userRegName     :: String
, userRegPassword :: String
, userRegRegTime  :: Day
} deriving (Eq)

deriveToRow ''RegisterUser

data User = User {
  userId       :: UserId
, userName     :: String
, userPassword :: String
, userRegTime  :: Day
} deriving (Eq)

deriveFromRow ''User
deriveToRow ''User

type UserAPI =
       ArgNamed "u" (Composite RegisterUser)
    :> Procedure "postUser" (Only Int)
  :<|> ArgPos Int
    :> Procedure "getUser" (Maybe User)
  :<|> ArgPos Int
    :> Procedure "deleteUser" ()
  :<|> Procedure "getUsers" [User]
```

3. Derive client functions from the API:

``` haskell
postUser :: Composite RegisterUser -> PostgresM (Only Int)
getUser :: Int -> PostgresM (Maybe User)
deleteUser :: Int -> PostgresM ()
getUsers :: PostgresM [User]
(      postUser
  :<|> getUser
  :<|> deleteUser
  :<|> getUsers) = deriveDB (Proxy :: Proxy UserAPI) (Proxy :: Proxy PostgresM)
```

4. Use them. The full example you can view at [example01/Main.hs](https://github.com/NCrashed/servant-db-postgresql/tree/master/example01/Main.hs) module. 
And to compile it run the:
```
stack build --flag servant-db-postgresql:examples
```
