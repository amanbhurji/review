{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}

module Server where

import           Data.Aeson
import           Data.Functor
import           Data.Foldable
import qualified Control.Monad.IO.Class        as C
import qualified Data.Text                     as T
import qualified Db
import           Models
import           Servant

type ReviewAPI2 = "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId
      :<|> "paste" :> Capture "id" PasteId :> Get '[JSON] (Maybe Paste)
      :<|> "paste" :> Capture "id" PasteId :> "comment" :> QueryParam "line" Int :> ReqBody '[JSON] T.Text :> Post '[JSON] NoContent
      :<|> "pages" :> Raw

server2 :: Db.Db -> Server ReviewAPI2
server2 db = newpaste
     :<|> getpaste
     :<|> newcomment
     :<|> staticpages
  where newpaste :: T.Text -> Handler PasteId
        newpaste content = do
          p <- C.liftIO $ newPaste content
          C.liftIO $ Db.writeToDb db p
          pure $ pasteId p

        getpaste :: PasteId -> Handler (Maybe Paste)
        getpaste pId = C.liftIO $ getPasteFromDb pId db

        newcomment :: PasteId -> Maybe Int -> T.Text -> Handler NoContent
        newcomment pId linenumber commenttxt = do
          maybePaste <- C.liftIO $ getPasteFromDb pId db
          let maybeUpdatePaste = newComment commenttxt (fmap LineNumber linenumber) <$> maybePaste
          let x = traverse_ (Db.writeToDb db) maybeUpdatePaste -- traverse_ because no meaningful value is produced
          C.liftIO x
          return NoContent

        staticpages = serveDirectoryWebApp "pages"

reviewAPI2 :: Proxy ReviewAPI2
reviewAPI2 = Proxy

app2 :: Db.Db -> Application
app2 db = serve reviewAPI2 $ server2 db

-- | Does lookup in some global state for `Paste`s
getPasteFromDb :: PasteId -> Db.Db -> IO (Maybe Paste)
getPasteFromDb pId = Db.lookupPaste pId pasteId

