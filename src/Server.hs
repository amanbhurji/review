{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson
import Models
import Servant

type ReviewAPI1 = "pastes" :> Get '[JSON] [Paste]

server1 :: Server ReviewAPI1
server1 = return pastes1

reviewAPI :: Proxy ReviewAPI1
reviewAPI = Proxy

app1 :: Application
app1 = serve reviewAPI server1

pastes1 :: [Paste]
pastes1 = [newPaste "This is a paste!"]