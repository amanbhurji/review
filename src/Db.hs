module Db where

import qualified Data.IORef as I
import Models

newtype Db = Db { allPastes :: I.IORef [Paste] }

makeDb :: [Paste] -> IO Db
makeDb init = do
        iref <- I.newIORef init
        pure (Db iref)

writeToDb :: Paste -> Db -> IO ()
writeToDb p (Db all) = I.modifyIORef all (p:)

debugShowDb :: Db -> IO ()
debugShowDb (Db all) = do
        all' <- I.readIORef all
        print all'
    