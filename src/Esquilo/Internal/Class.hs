{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
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

data (a :: Symbol) :|: (b :: Symbol)

class Select a where
    type SelectT a :: k
    select :: Proxy a -> Connection -> IO [SelectT a]

instance ( KnownSymbol x, KnownSymbol tbl, Field x tbl ~ r, FromField r
         ) => Select (SELECT x FROM tbl) where
    type SelectT (SELECT x FROM tbl) = Field x tbl
    select _ = \conn -> fromOnly <$$> (query_ conn $ "SELECT " <> x <> " FROM " <> tbl)
      where
        x   = fromString $ symbolVal (Proxy :: Proxy x)
        tbl = fromString $ symbolVal (Proxy :: Proxy tbl)

instance ( KnownSymbol x, KnownSymbol y, KnownSymbol tbl, Field x tbl ~ r
         , Field y tbl ~ r', FromRow (r, r')
        ) => Select (SELECT (x :|: y) FROM tbl) where
    type SelectT (SELECT (x :|: y) FROM tbl) = (Field x tbl, Field y tbl)
    select _ = \conn -> query_ conn q
      where
        q   = "SELECT " <> x <> ", " <> y <> " FROM " <> tbl
        x   = fromString $ symbolVal (Proxy :: Proxy x)
        y   = fromString $ symbolVal (Proxy :: Proxy y)
        tbl = fromString $ symbolVal (Proxy :: Proxy tbl)


type family Field (f :: Symbol) (tbl :: Symbol)

(<$$>) = fmap . fmap
