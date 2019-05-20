{-# Language OverloadedStrings #-}

module Main where

import Db.PostgresqlSimple (testConnection)

main :: IO ()
main = testConnection
