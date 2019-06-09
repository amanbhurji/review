{-# Language OverloadedStrings #-}

module Main where

import           Data.Either (fromRight)
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Network.Wai.Middleware.Cors
import           Server

import qualified Db.Hasql as H

main :: IO ()
main = do
    conn <- H.dbconn
    let conn' = fromRight undefined conn
    withStdoutLogger $ \aplogger -> do
      let settings = setPort 8081 $ setLogger aplogger defaultSettings
      runSettings settings $ handleCors $ app2 conn'

handleCors = cors ( const $
  Just (simpleCorsResourcePolicy
    { corsRequestHeaders = ["Content-Type"]
    }) )

