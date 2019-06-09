module PostgresApi
  ( lookupPaste
  , writePaste
  , addComment
  , runSession
  ) where

import qualified Control.Monad.IO.Class as C
import qualified Db.Hasql
import qualified Db.Schemas as Sc
import           Models ( LineNumber(..)
                        , Paste
                        , newPasteWithUUID
                        , newComment
                        )
import           Hasql.Connection ( Connection
                                  , ConnectionError
                                  )
import           Hasql.Session ( Session
                               , run)
import           Data.Either ( fromRight )
import           Data.Functor ( ($>) )
import           Data.Int
import qualified Data.Text as T
import qualified Data.UUID as DU
import           Data.UUID.V4 ( nextRandom )
import           Data.Vector ( Vector )
import qualified Data.Vector as V

-- Discard error and get result. poo
runSession :: Connection -> Session b -> IO b
runSession conn s = fmap (fromRight undefined) res
  where res = run s conn

lookupPaste :: DU.UUID -> Session Paste
lookupPaste pid = do
  paste <- Db.Hasql.selectPasteByPidSession pid
  comments <- Db.Hasql.selectCommentsByPidSession pid
  return $ convertPaste paste comments

writePaste :: T.Text -> IO (Session DU.UUID)
writePaste txt =
  (\x -> writePasteWithId txt x $> x) <$> pid
  where pid = nextRandom

writePasteWithId :: T.Text -> DU.UUID -> Session ()
writePasteWithId txt uuid =
  Db.Hasql.insertPasteSession (Sc.Paste uuid txt)

addComment :: DU.UUID -> Int -> T.Text -> Session ()
addComment uuid lno txt = do
  comments <- Db.Hasql.selectCommentsByPidSession uuid
  let lineCommentsLength = length $ filterCommentsForLine lno comments
  -- race condition or session building?
  addCommentWithFields uuid lno (lineCommentsLength + 1) txt

int16ToInt :: Int16 -> Int
int16ToInt = fromIntegral

intToInt16 :: Int -> Int16
intToInt16 = fromIntegral

filterCommentsForLine :: Int -> Vector Sc.Comment -> Vector Sc.Comment
filterCommentsForLine lno comments =
  V.filter (((intToInt16 lno) ==) . Sc.c_lno) comments

addCommentWithFields :: DU.UUID -> Int -> Int -> T.Text -> Session ()
addCommentWithFields uuid lno cno txt =
  Db.Hasql.insertCommentSession
    (Sc.Comment uuid (intToInt16 lno) (intToInt16 cno) txt)

convertPaste :: Sc.Paste -> Vector Sc.Comment -> Paste
convertPaste dbpaste dbcomments =
  foldr addCommentToPaste pasteWithoutComments dbcomments
  where
    pasteWithoutComments =
      newPasteWithUUID (Sc.p_body dbpaste) (Sc.p_pid dbpaste)

addCommentToPaste :: Sc.Comment -> Paste -> Paste
addCommentToPaste c =
  newComment
    (Sc.c_body c)
    (Just $ LineNumber $ int16ToInt $ Sc.c_lno c)

