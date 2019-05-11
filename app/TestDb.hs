{-# Language OverloadedStrings #-}

module Main where

import DbPostgresqlSimple (testConnection)

main :: IO ()
main = testConnection
