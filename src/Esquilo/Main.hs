{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Esquilo.Main where

import Data.Proxy
import Database.PostgreSQL.Simple
import Esquilo.Internal.Class
import Esquilo.Types.Keywords

-- Specfic table description
type instance Field "name" "person" = String
type instance Field "age"  "person" = Int

type TestQuery1 = SELECT "name" FROM "person"
type TestQuery2 = SELECT "age" FROM "person"
{-type TestQuery3 = SELECT ["age", "name"] FROM "person"-}

t1 :: IO ()
t1 = do
    conn <- connectPostgreSQL "dbname=esquilo_test"
    res1 :: _ <- select (Proxy :: Proxy TestQuery1) conn
    print res1
    res2 :: _ <- select (Proxy :: Proxy TestQuery2) conn
    print res2
