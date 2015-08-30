{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module Esquilo.Internal.Class where

import Data.Monoid
import Data.Proxy
import Data.String
import Esquilo.Types.Keywords
import Esquilo.Types.Types
import GHC.TypeLits
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField hiding (Field)

class Select a where
    type SelectT a
    select :: Proxy a -> Connection -> IO [SelectT a]

instance ( KnownSymbol x, KnownSymbol tbl, Field x tbl ~ r, FromField r
         ) => Select (SELECT x FROM tbl) where
    type SelectT (SELECT x FROM tbl) = Field x tbl
    select _ = \conn -> fromOnly <$$> (query_ conn $ "SELECT " <> x <> " FROM " <> tbl)
      where
        x   = fromString $ symbolVal (Proxy :: Proxy x)
        tbl = fromString $ symbolVal (Proxy :: Proxy tbl)

{-instance ( Select (SELECT xs FROM tbl), KnownSymbol x, KnownSymbol tbl-}
        {-) => Select (SELECT (x ': xs) FROM tbl) where-}
    {-type SelectT (SELECT (x ': xs) FROM tbl) = Field x tbl ': (SelectT xs tbl)-}

type family Field (f :: Symbol) (tbl :: Symbol)

(<$$>) = fmap . fmap
