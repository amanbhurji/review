module Db where

import qualified Data.Maybe as Maybe    
import qualified Data.IORef as I
import Data.Foldable (find)
import Models

newtype Db = Db { allPastes :: I.IORef [Paste] }

makeDb :: [Paste] -> IO Db
makeDb init = do
  iref <- I.newIORef init
  pure (Db iref)

writeToDb :: Db -> Paste -> IO ()
writeToDb (Db all) p = I.modifyIORef all (p:)

lookupPaste :: Eq t => t -> (Paste -> t) -> Db -> IO (Maybe Paste)
lookupPaste t f (Db all)= do
  all' <- I.readIORef all
  pure $ findFirst t f all'

debugGetAllDb :: Db -> IO [Paste]
debugGetAllDb (Db all) = I.readIORef all

debugShowDb :: Db -> IO ()
debugShowDb db = do
  all <- debugGetAllDb db
  print all

findFirst :: (Eq s, Foldable m) =>
  s -> (t -> s) -> m t -> Maybe t
findFirst s f = find ((s ==) . f)