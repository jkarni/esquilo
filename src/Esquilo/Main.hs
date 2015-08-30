{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Esquilo.Main where

import           Data.Proxy
import           Database.PostgreSQL.Simple
import           Esquilo.Internal.Class
import           Esquilo.Types.Keywords

-- Specfic table description
type instance Field "name" "person" = String
type instance Field "age"  "person" = Int

type TestQuery1 = SELECT "name" FROM "person"
type TestQuery2 = SELECT "age" FROM "person"
type TestQuery3 = SELECT ("age" :|: "name") FROM "person"
{-type TestQuery4 = SELECT "name" FROM "person" WHERE ["age" := 5]-}

t1 :: IO ()
t1 = do
    conn <- connectPostgreSQL "dbname=esquilo_test"
    res1 <- select (Proxy :: Proxy TestQuery1) conn
    print res1
    res2 <- select (Proxy :: Proxy TestQuery2) conn
    print res2
    res3 <- select (Proxy :: Proxy TestQuery3) conn
    print res3
