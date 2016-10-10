{-|
Module      : Servant.DB.PostgreSQL
Description : Reexport tools for deriving of PostgreSQL client for DB API.
Portability : Not portable
-}
module Servant.DB.PostgreSQL(
    module Reexport
  ) where

import           Servant.DB.PostgreSQL.Composite as Reexport
import           Servant.DB.PostgreSQL.HasDB     as Reexport
