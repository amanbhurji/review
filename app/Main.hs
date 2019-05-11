{-# Language OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Network.Wai.Logger
import Network.Wai.Middleware.Cors
import Server
import qualified Db

main :: IO ()
main = do
    db <- Db.makeDb []
    withStdoutLogger $ \aplogger -> do
      let settings = setPort 8081 $ setLogger aplogger defaultSettings
      runSettings settings $ handleCors $ app2 db

handleCors = cors ( const $
  Just (simpleCorsResourcePolicy
    { corsRequestHeaders = ["Content-Type"]
    }) )

