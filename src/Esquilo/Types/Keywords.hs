{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Esquilo.Types.Keywords where
import GHC.TypeLits

data SELECT (a :: k) from (tbl :: Symbol)
data FROM

