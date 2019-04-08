{-# Language OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Server
import qualified Db

main :: IO ()
main = do
    db <- Db.makeDb []
    run 8081 $ handleCors $ app2 db

handleCors = cors ( const $
  Just (simpleCorsResourcePolicy
    { corsRequestHeaders = ["Content-Type"]
    }) )

