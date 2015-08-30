module Esquilo.Types.Types where

import qualified Database.PostgreSQL.Simple as PSQL

newtype Q a = Q { getQuery :: PSQL.Query }
    deriving (Eq, Show)
