{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Models
  ( Paste
  , PasteId
  -- make a smart constructor which only creates LineNumbers for lines
  -- that exist in a Paste instead of exposing this constructor
  , LineNumber(..)
  , newPaste
  , newComment
  , pasteId
  ) where

import           Control.Lens   ((^?), (&), (.~), ix)
import qualified Data.Maybe     as M
import qualified Data.UUID      as DU
import           Data.UUID.V4   (nextRandom)
import           Data.Aeson
import qualified Data.Text      as T
import           GHC.Generics

data Paste = Paste
  { _lines    :: [LineWithComments]
  , _comments :: [TopLevelComment] -- reversed, newest comment = head
  , _id       :: PasteId
  } deriving (Eq, Show, Generic)

instance ToJSON Paste where
  toEncoding Paste{..} =
    pairs ("id" .= _id <> "lines" .= _lines <> "comments" .= _comments)

type PasteId = DU.UUID

data LineWithComments = LineWithComments
  { _line :: Line
  , _lineComments :: [LineLevelComment]
  } deriving (Eq, Show, Generic)

instance ToJSON LineWithComments where
  toEncoding LineWithComments{..} =
    pairs ("line" .= _line <> "comments" .= _lineComments)

newtype Line = Line T.Text deriving (Eq, Show, Generic)

instance ToJSON Line where
  toEncoding (Line txt) = toEncoding txt

data Comment = Comment
  { _body   :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Comment where
  toEncoding Comment{..} =
    pairs ("body" .= _body)

type LineLevelComment = Comment
type TopLevelComment  = Comment

newtype LineNumber = LineNumber Int deriving (Eq, Show)

pasteId :: Paste -> PasteId
pasteId = _id

newPasteWithUUID :: T.Text -> PasteId -> Paste
newPasteWithUUID content =
  Paste (fmap (flip LineWithComments [] . Line) (T.lines content)) []

newPaste :: T.Text -> IO Paste
newPaste content = newPasteWithUUID content <$> pId
  where pId = nextRandom

newComment :: T.Text -> Maybe LineNumber -> Paste -> Paste
newComment comment maybeLineNumber paste =
  case maybeLineNumber of
    Just n  -> paste { _lines = addCommentAtLine n (_lines paste) (Comment comment) }
    Nothing -> paste { _comments = (Comment comment) : (_comments paste) }

addCommentAtLine
  :: LineNumber
  -> [LineWithComments]
  -> LineLevelComment
  -> [LineWithComments]
addCommentAtLine (LineNumber n) ls comment = M.fromMaybe [] updatedLs where
  updatedLs    = fmap (\newLine -> ls & ix n .~ newLine) maybeNewLine
  maybeOldLine = ls ^? ix n
  maybeNewLine = fmap (addCommentToLine comment) maybeOldLine

addCommentToLine :: LineLevelComment -> LineWithComments -> LineWithComments
addCommentToLine comment lineWithComments =
  lineWithComments { _lineComments = newComments }
    where
      oldComments = _lineComments lineWithComments
      newComments = comment : oldComments

{-
newComment :: T.Text -> Maybe LineNumber -> Paste -> Paste
newComment comment maybeLineNumber paste =
  Paste (_content paste) (newComment' : _comments paste) (_id paste)
    where
      newComment' = Comment comment anchor
      anchor = maybe TopLevel Line maybeLineNumber
-}
