{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson
import Data.Functor
import Debug.Trace
import qualified Control.Monad.IO.Class as C
import qualified Data.Text as T
import qualified Db
import Models
import Servant

-- type ReviewAPI1 = "pastes" :> Get '[JSON] [Paste]

type ReviewAPI2 = "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId
      :<|> "paste" :> Capture "id" PasteId :> Get '[JSON] (Maybe Paste)
      :<|> "paste" :> Capture "id" PasteId :> "comment" :> QueryParam "line" Int :> ReqBody '[JSON] T.Text :> Post '[JSON] NoContent
      :<|> "pastes" :> Get '[JSON] [Paste]

-- server1 :: Server ReviewAPI1
-- server1 = return pastes1

server2 :: Db.Db -> Server ReviewAPI2
server2 db = newpaste
     :<|> getpaste
     :<|> newcomment
     :<|> getAllPastes
  where newpaste :: T.Text -> Handler PasteId
      --   newpaste content = return $ pasteId newone
      --     where newone = newPaste content
        newpaste content = do
          p <- C.liftIO $ newPaste content
          C.liftIO $ Db.writeToDb db p
          pure $ pasteId p

        getpaste :: PasteId -> Handler (Maybe Paste)
        getpaste pId = C.liftIO $ getPasteFromDb pId db

        newcomment :: PasteId -> Maybe Int -> T.Text -> Handler NoContent
        newcomment pId linenumber comment = do
          maybePaste <- C.liftIO $ getPasteFromDb (trace ("pid: " <> show pId) pId) db
          let maybeUpdatedPaste = newComment comment (fmap LineNumber linenumber) <$> (trace ("paste:" <> show maybePaste) maybePaste)
          return (Db.writeToDb db <$> maybeUpdatedPaste) $> NoContent

        getAllPastes :: Handler [Paste]
        getAllPastes = C.liftIO $ Db.debugGetAllDb db

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
getPasteFromDb :: PasteId -> Db.Db -> IO (Maybe Paste)
getPasteFromDb pId = Db.lookupPaste pId pasteId
