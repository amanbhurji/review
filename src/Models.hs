{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models 
  ( Paste
  , PasteId
  , LineNumber
  , newPaste
  , newFileComment
  , newLineComment
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Data.Time.Clock
import qualified Data.UUID as Uuid

data Paste = Paste 
  { _content  :: Content
  , _comments :: [Comment] -- reversed, last comment = head
  , _id       :: PasteId
  } deriving (Eq, Show, Generic, ToJSON)

type PasteId = Uuid.UUID

genPasteId :: Maybe PasteId
genPasteId = Uuid.fromString $ show getCurrentTime

newtype Content = Content T.Text deriving (Eq, Show, Generic, ToJSON)

data Comment = Comment 
  { _body   :: T.Text
  , _anchor :: Anchor
  , _timeStamp :: UTCTime
  } deriving (Eq, Show, Generic, ToJSON)

data Anchor = TopLevel | Line LineNumber deriving (Eq, Show, Generic, ToJSON)

newtype LineNumber = LineNumber Int deriving (Eq, Show, Generic, ToJSON)

id :: Paste -> PasteId
id = _id

-- stuck on this. I want to return a paste if genPasteId is a Just PasteId otherwise nothing
newPaste :: T.Text -> Maybe Paste
newPaste input = do
  Paste input [] <$> genPasteId

newFileComment :: T.Text -> Paste -> Paste
newFileComment = undefined

newLineComment :: T.Text -> LineNumber -> Paste -> Paste
newLineComment = undefined
