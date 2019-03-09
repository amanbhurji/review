module Db where

import qualified Data.Maybe as Maybe    
import qualified Data.IORef as I
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

debugShowDb :: Db -> IO ()
debugShowDb (Db all) = do
  all' <- I.readIORef all
  print all'

findFirst :: (Eq s, Eq t) => s -> (t -> s) -> [t] -> Maybe t
findFirst _ _ [] = Nothing
findFirst s f ts = foldl f' Nothing ts
  where f' acc t' = if Maybe.isNothing acc && (f t' == s) then Just t' else acc
