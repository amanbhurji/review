{-# LANGUAGE OverloadedStrings #-}

module Db.PostgresqlSimple where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

testConnection = do
  conn <- connect defaultConnectInfo {
    connectPassword = "password"
  }
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "SELECT 2 + 2" :: IO [Only Int] )
  putStrLn "3 + 5"
  mapM_ print =<< ( query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int] )

