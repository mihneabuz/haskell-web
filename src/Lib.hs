module Lib
  ( main,
    Config (Config),
  )
where

import Postgres (getPostgres)
import Routes (routes)
import Types (Config (Config))
import Web.Scotty (scotty)

main :: IO ()
main = do
  pg <- getPostgres

  putStrLn "starting server"
  scotty 3000 $ routes $ Config pg
