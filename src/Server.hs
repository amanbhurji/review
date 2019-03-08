{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson
import qualified Data.Text as T
import Models
import Servant

-- type ReviewAPI1 = "pastes" :> Get '[JSON] [Paste]

type ReviewAPI2 = "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId
      :<|> "paste" :> Capture "id" PasteId :> Get '[JSON] Paste
      :<|> "paste" :> Capture "id" PasteId :> "comment" :> QueryParam "line" Int :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId

-- server1 :: Server ReviewAPI1
-- server1 = return pastes1

server2 :: Server ReviewAPI2
server2 = newpaste
     :<|> getpaste
     :<|> newcomment
  where newpaste :: T.Text -> Handler PasteId
        newpaste content = return $ pasteId (newPaste content)

        getpaste :: PasteId -> Handler Paste
        getpaste = return . getPaste

        -- | Update this to handle line comments and file comments. Possible api change required
        newcomment :: PasteId -> Maybe Int -> T.Text -> Handler PasteId
        newcomment pid linenumber comment = return $ pasteId (newComment comment (fmap LineNumber linenumber) (getPaste pid))

-- reviewAPI1 :: Proxy ReviewAPI1
-- reviewAPI1 = Proxy

reviewAPI2 :: Proxy ReviewAPI2
reviewAPI2 = Proxy

-- app1 :: Application
-- app1 = serve reviewAPI1 server1

app2 :: Application
app2 = serve reviewAPI2 server2

pastes1 :: [Paste]
pastes1 = [newPaste "This is a paste!"]

-- | Does lookup in some global state for `Paste`s
--   This signature will likely change
getPaste :: PasteId -> Paste
getPaste = undefined
