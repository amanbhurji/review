{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson
import Data.Functor
import qualified Control.Monad.IO.Class as C
import qualified Data.Text as T
import qualified Db
import Models
import Servant

-- type ReviewAPI1 = "pastes" :> Get '[JSON] [Paste]

type ReviewAPI2 = "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId
      :<|> "paste" :> Capture "id" PasteId :> Get '[JSON] Paste
      :<|> "paste" :> Capture "id" PasteId :> "comment" :> QueryParam "line" Int :> ReqBody '[JSON] T.Text :> Post '[JSON] NoContent

-- server1 :: Server ReviewAPI1
-- server1 = return pastes1

server2 :: Db.Db -> Server ReviewAPI2
server2 db = newpaste
     :<|> getpaste
     :<|> newcomment
  where newpaste :: T.Text -> Handler PasteId
      --   newpaste content = return $ pasteId newone
      --     where newone = newPaste content
        newpaste content = do
            C.liftIO $ Db.writeToDb newone db
            return $ pasteId newone
            where newone = newPaste content

        getpaste :: PasteId -> Handler Paste
        getpaste = return . getPaste

        -- | Update this to handle line comments and file comments. Possible api change required
        newcomment :: PasteId -> Maybe Int -> T.Text -> Handler NoContent
        newcomment pid linenumber comment =
          return (newComment comment (fmap LineNumber linenumber) (getPaste pid)) $> NoContent

-- reviewAPI1 :: Proxy ReviewAPI1
-- reviewAPI1 = Proxy

reviewAPI2 :: Proxy ReviewAPI2
reviewAPI2 = Proxy

-- app1 :: Application
-- app1 = serve reviewAPI1 server1

app2 :: Db.Db -> Application
app2 db = serve reviewAPI2 $ server2 db

-- pastes1 :: [Paste]
-- pastes1 = [newPaste "This is a paste!"]

-- | Does lookup in some global state for `Paste`s
--   This signature will likely change
getPaste :: PasteId -> Paste
getPaste = undefined
