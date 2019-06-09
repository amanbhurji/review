{-# LANGUAGE OverloadedStrings #-}

module Db.Hasql where

import           Data.Either ( fromRight )
import           Data.Functor.Contravariant
import           Data.Int
import qualified Data.UUID as DU
import           Data.Vector (Vector)
import qualified Hasql.Connection as Conn
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Stmt
import qualified Hasql.Session as Session

import           Db.SqlStmts ( createDbSql
                             , createPastesTableSql
                             , createCommentsTableSql
                             , insertPastePrepared
                             , insertCommentPrepared
                             , selectPasteByPidSql
                             , selectCommentsByPidSql
                             )
import qualified Db.Schemas as Schemas

dbsettings = Conn.settings "localhost" 5432 "postgres" "password" "pastes"

dbconn :: IO (Either Conn.ConnectionError Conn.Connection)
dbconn = Conn.acquire dbsettings

insertCommentSession :: Schemas.Comment -> Session.Session ()
insertCommentSession comment =
  Session.statement comment insertCommentStmt

insertPasteSession :: Schemas.Paste -> Session.Session ()
insertPasteSession paste =
  Session.statement paste insertPasteStmt

selectPasteByPidSession :: DU.UUID -> Session.Session Schemas.Paste
selectPasteByPidSession pid =
  Session.statement pid selectPasteByPidStmt

selectCommentsByPidSession :: DU.UUID -> Session.Session (Vector Schemas.Comment)
selectCommentsByPidSession pid =
  Session.statement pid selectCommentsByPidStmt

insertPasteStmt =
  Stmt.Statement insertPastePrepared encoder decoder True
    where
      encoder =
        contramap Schemas.p_pid (Encoders.param (Encoders.nonNullable Encoders.uuid)) <>
        contramap Schemas.p_body (Encoders.param (Encoders.nonNullable Encoders.text))
      decoder =
        Decoders.noResult

insertCommentStmt =
  Stmt.Statement insertCommentPrepared encoder decoder True
    where
      -- Order of elements should be same as in insertCommentPrepared
      encoder =
        contramap Schemas.c_pid (Encoders.param (Encoders.nonNullable Encoders.uuid)) <>
        contramap Schemas.c_lno (Encoders.param (Encoders.nonNullable Encoders.int2)) <>
        contramap Schemas.c_cno (Encoders.param (Encoders.nonNullable Encoders.int2)) <>
        contramap Schemas.c_body (Encoders.param (Encoders.nonNullable Encoders.text))
      decoder =
        Decoders.noResult

selectPasteByPidStmt =
  Stmt.Statement selectPasteByPidSql encoder decoder True
    where
      encoder =
        Encoders.param (Encoders.nonNullable Encoders.uuid)
      decoder =
        Decoders.singleRow
          (Schemas.Paste <$> (Decoders.column . Decoders.nonNullable) Decoders.uuid <*>
            (Decoders.column . Decoders.nonNullable) Decoders.text)

selectCommentsByPidStmt =
  Stmt.Statement selectCommentsByPidSql encoder decoder True
    where
      encoder =
        Encoders.param (Encoders.nonNullable Encoders.uuid)
      decoder =
        Decoders.rowVector
          (Schemas.Comment <$>
            (Decoders.column . Decoders.nonNullable) Decoders.uuid <*>
            (Decoders.column . Decoders.nonNullable) Decoders.int2 <*>
            (Decoders.column . Decoders.nonNullable) Decoders.int2 <*>
            (Decoders.column . Decoders.nonNullable) Decoders.text)

{- Can't connect to pastes db if it doesn't exist.
   Too much work to first create a connection to create pastes
   then switch connection to pastes and create the tables in it.
-}
setupTables = do
  -- let sessDb = Session.sql createDbSql
  let sessPastesTable = Session.sql createPastesTableSql
  let sessCommentsTable = Session.sql createCommentsTableSql
  -- e1 <- dbrun sessDb
  e2 <- dbrun sessPastesTable
  e3 <- dbrun sessCommentsTable
  return $ e2 *> e3

dbrun :: Session.Session a -> IO (Either Session.QueryError a)
dbrun sess = do
  c <- dbconn
  let c' = fromRight undefined c
  Session.run sess c'

