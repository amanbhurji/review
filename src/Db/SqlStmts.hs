{-# LANGUAGE OverloadedStrings #-}

module Db.SqlStmts where

import qualified Data.String as S

createDbSql :: S.IsString a => a
createDbSql = "CREATE DATABASE pastes;"

dropAndCreateDbSql :: S.IsString a => a
dropAndCreateDbSql = "DROP DATABASE IF EXISTS pastes; CREATE DATABASE pastes;"

createPastesTableSql :: S.IsString a => a
createPastesTableSql = "CREATE TABLE pastes ( pid UUID PRIMARY KEY, body Text NOT NULL );"

createCommentsTableSql :: S.IsString a => a
createCommentsTableSql = "CREATE TABLE comments ( line_number SMALLINT NOT NULL, comment_number SMALLINT NOT NULL, body TEXT NOT NULL, pid UUID REFERENCES pastes(pid) ON DELETE CASCADE, PRIMARY KEY (pid, line_number, comment_number) );"

insertPastePrepared :: S.IsString a => a
insertPastePrepared = "INSERT INTO pastes ( pid, body ) VALUES ( $1, $2 )"

insertCommentPrepared :: S.IsString a => a
insertCommentPrepared = "INSERT INTO comments ( pid, line_number, comment_number, body ) VALUES ( $1, $2, $3, $4 )"

selectByPid :: (Semigroup a, S.IsString a) => a -> a
selectByPid tableName = "SELECT * FROM " <> tableName <> " WHERE pid=$1"

selectPasteByPid :: (Semigroup a, S.IsString a) => a
selectPasteByPid = selectByPid "pastes"

selectCommentsByPid :: (Semigroup a, S.IsString a) => a
selectCommentsByPid = selectByPid "comments"

selectPasteWithComments :: S.IsString a => a
selectPasteWithComments = "SELECT * FROM pastes p LEFT OUTER JOIN comments c ON p.pid = c.pid WHERE p.pid=$1;"

