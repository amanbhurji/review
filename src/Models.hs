{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Paste = Paste 
  { _content  :: Content
  , _comments :: [Comment] -- reversed, last comment = head
  , _id       :: PasteId
  } deriving (Eq, Show, Generic, ToJSON)

type PasteId = Int -- use a better type 

newtype Content = Content T.Text deriving (Eq, Show, Generic, ToJSON)

data Comment = Comment 
  { _body   :: T.Text
  , _anchor :: Anchor
  } deriving (Eq, Show, Generic, ToJSON)

data Anchor = TopLevel | Line LineNumber deriving (Eq, Show, Generic, ToJSON)

newtype LineNumber = LineNumber Int deriving (Eq, Show, Generic, ToJSON)

pasteId :: Paste -> PasteId
pasteId = _id

newPaste :: T.Text -> Paste
newPaste = undefined

newFileComment :: T.Text -> Paste -> Paste
newFileComment = undefined

newLineComment :: T.Text -> LineNumber -> Paste -> Paste
newLineComment = undefined

newComment :: T.Text -> Maybe LineNumber -> Paste -> Paste
newComment comment maybeLineNumber paste =
  Paste (_content paste) (newComment': (_comments paste)) (_id paste)
    where
      newComment' = Comment comment anchor
      anchor = maybe TopLevel Line maybeLineNumber