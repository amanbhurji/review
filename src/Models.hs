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

import qualified  Data.UUID     as DU
import            Data.UUID.V4  (nextRandom)
import            Data.Aeson
import qualified  Data.Text     as T
import            GHC.Generics

data Paste = Paste 
  { _content  :: Content
  , _comments :: [Comment] -- reversed, last comment = head
  , _id       :: PasteId
  } deriving (Eq, Show, Generic)

instance ToJSON Paste where
  toEncoding Paste{..} =
    pairs ("id" .= _id <> "content" .= _content <> "comments" .= _comments)

type PasteId = DU.UUID

newtype Content = Content T.Text deriving (Eq, Show, Generic)

instance ToJSON Content where
  toEncoding (Content txt) = toEncoding txt

data Comment = Comment
  { _body   :: T.Text
  , _anchor :: Anchor
  } deriving (Eq, Show, Generic)

instance ToJSON Comment where
  toEncoding Comment{..} =
    pairs ("body" .= _body <> "anchor" .= _anchor)

data Anchor = TopLevel | Line LineNumber deriving (Eq, Show, Generic)

instance ToJSON Anchor where
  toEncoding TopLevel = toEncoding ("toplevel" :: String)
  toEncoding (Line n) = pairs ("line" .= n)

newtype LineNumber = LineNumber Int deriving (Eq, Show, Generic)

instance ToJSON LineNumber where
  toEncoding (LineNumber n) = toEncoding n

pasteId :: Paste -> PasteId
pasteId = _id

newPasteWithUUID :: T.Text -> PasteId -> Paste
newPasteWithUUID content = Paste (Content content) []

newPaste :: T.Text -> IO Paste
newPaste content = newPasteWithUUID content <$> pId
  where pId = nextRandom

newComment :: T.Text -> Maybe LineNumber -> Paste -> Paste
newComment comment maybeLineNumber paste =
  Paste (_content paste) (newComment' : _comments paste) (_id paste)
    where
      newComment' = Comment comment anchor
      anchor = maybe TopLevel Line maybeLineNumber
