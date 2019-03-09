module Main where

import Network.Wai.Handler.Warp
import Server
import qualified Db

main :: IO ()
main = do
    db <- Db.makeDb []
    run 8081 $ app2 db
