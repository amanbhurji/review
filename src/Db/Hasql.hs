{-# LANGUAGE OverloadedStrings #-}

module Db.Hasql where

import qualified Data.Either as E
import           Data.Functor.Contravariant
import           Data.Int
import qualified Hasql.Connection as Conn
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Statement as Stmt
import qualified Hasql.Session as Session
import           Db.SqlStmts (createDbSql, createPastesTableSql, createCommentsTableSql, insertPastePrepared, insertCommentPrepared, selectPasteByPidSql, selectCommentsByPidSql)
import qualified Db.Schemas as Schemas

dbsettings = Conn.settings "localhost" 5432 "postgres" "password" "pastes"

dbconn = Conn.acquire dbsettings

insertPasteStmt =
  Stmt.Statement insertPastePrepared encoder decoder True
    where
      encoder =
        contramap Schemas.p_pid (Encoders.param Encoders.uuid) <>
        contramap Schemas.p_body (Encoders.param Encoders.text)
      decoder =
        Decoders.unit

insertPasteSession paste = Session.statement paste insertPasteStmt

insertCommentStmt =
  Stmt.Statement insertCommentPrepared encoder decoder True
    where
      -- Order of elements should be same as in insertCommentPrepared
      encoder =
        contramap Schemas.c_pid (Encoders.param Encoders.uuid) <>
        contramap Schemas.c_lno (Encoders.param Encoders.int2) <>
        contramap Schemas.c_cno (Encoders.param Encoders.int2) <>
        contramap Schemas.c_body (Encoders.param Encoders.text)
      decoder =
        Decoders.unit

insertCommentSession comment = Session.statement comment insertCommentStmt

selectPasteByPidStmt =
  Stmt.Statement selectPasteByPidSql encoder decoder True
    where
      encoder =
        Encoders.param Encoders.uuid
      decoder =
        Decoders.singleRow
          (Schemas.Paste <$> Decoders.column Decoders.uuid <*>
            Decoders.column Decoders.text)

selectPasteByPidSession pid = Session.statement pid selectPasteByPidStmt

selectCommentsByPidStmt =
  Stmt.Statement selectCommentsByPidSql encoder decoder True
    where
      encoder =
        Encoders.param Encoders.uuid
      decoder =
        Decoders.rowVector
          (Schemas.Comment <$>
            Decoders.column Decoders.uuid <*>
            Decoders.column Decoders.int2 <*>
            Decoders.column Decoders.int2 <*>
            Decoders.column Decoders.text)

selectCommentsByPidSession pid = Session.statement pid selectCommentsByPidStmt

{- Can't connect to pastes db if it doesn't exist.
   Too much work to first create a connection to create pastes
   then switch connection to pastes and create the tables in it.
-}
setupTables = do
  -- let sessDb = Session.sql createDbSql
  let sessPastesT = Session.sql createPastesTableSql
  let sessCommentsT = Session.sql createCommentsTableSql
  -- e1 <- dbrun sessDb
  e2 <- dbrun sessPastesT
  e3 <- dbrun sessCommentsT
  return $ e2 *> e3

dbrun sess = do
  c <- dbconn
  let c' = E.fromRight undefined c
  Session.run sess c'

