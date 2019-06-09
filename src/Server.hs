{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeOperators  #-}

module Server where

import           Data.Aeson
import           Data.Functor
import           Data.Foldable
import qualified Control.Monad.IO.Class        as C
import qualified Data.Text                     as T
import           Hasql.Connection ( Connection )
import           Models ( PasteId
                        , Paste
                        , LineNumber(..)
                        , newPaste
                        , pasteId
                        , newComment
                        )
import           PostgresApi ( addComment
                             , lookupPaste
                             , writePaste
                             , runSession
                             )
import           Servant

type ReviewAPI2 =
  "paste" :> ReqBody '[JSON] T.Text :> Post '[JSON] PasteId
  :<|> "paste" :> Capture "id" PasteId :> Get '[JSON] (Maybe Paste)
  :<|> "paste" :> Capture "id" PasteId :> "comment" :> QueryParam "line" Int :> ReqBody '[JSON] T.Text :> Post '[JSON] NoContent
  :<|> "pages" :> Raw

server2 :: Connection -> Server ReviewAPI2
server2 conn = newpaste
     :<|> getpaste
     :<|> newcomment
     :<|> staticpages
  where newpaste :: T.Text -> Handler PasteId
        newpaste content = do
          sp <- C.liftIO $ writePaste content
          C.liftIO $ runSession conn sp

        getpaste :: PasteId -> Handler (Maybe Paste)
        getpaste pId = C.liftIO $ Just <$> (runSession conn $ lookupPaste pId)

        newcomment :: PasteId -> Maybe Int -> T.Text -> Handler NoContent
        -- partial. ignoring case Nothing
        newcomment pId (Just lno) txt = x $> NoContent
          where x = C.liftIO $ runSession conn y
                y = addComment pId lno txt

        staticpages = serveDirectoryWebApp "pages"

reviewAPI2 :: Proxy ReviewAPI2
reviewAPI2 = Proxy

app2 :: Connection -> Application
app2 conn = serve reviewAPI2 $ server2 conn

