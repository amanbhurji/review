{-# LANGUAGE OverloadedStrings #-}

module DbPostgresqlSimple where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

testConnection = do
  conn <- connect defaultConnectInfo {
    connectPassword = "password"
  }
  putStrLn "2 + 2"
  mapM_ print =<< (query_ conn "SELECT 2 + 2" :: IO [Only Int] )

