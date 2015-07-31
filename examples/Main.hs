module Main where

import Control.Monad ( void )
import Data.Monoid
import Data.String
import Data.Yaml
import Database.PostgreSQL.Config
import Database.PostgreSQL.Simple
import System.Environment



main :: IO ()
main = do
    conf <- decodeFile "pgconfig.yml"
            >>= maybe (fail "Oups!") return
    pool <- getArgs >>= \case
      [] -> createPGPool conf
      [spath] ->
        let callback con =
              void
              $ execute_ con
              $ "SET search_path TO " <> fromString spath
        in createPGPoolWithCallback conf callback
      _ -> fail "wrong arguments"
    putStrLn "Pool created"
    pingPGPool pool
    putStrLn "Connection performed"
