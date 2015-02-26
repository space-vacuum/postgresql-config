module Main where

import Data.Yaml
import Database.PostgreSQL.Config


main :: IO ()
main = do
    conf <- decodeFile "pgconfig.yml"
            >>= maybe (fail "Oups!") return
    pool <- createPGPool conf
    putStrLn "Pool created"
    pingPGPool pool
    putStrLn "Connection performed"
