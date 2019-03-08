module Models where

import qualified Data.Text as T

data Paste = Paste 
  { _content  :: Content
  , _comments :: [Comment] -- reversed, last comment = head
  , _id       :: PasteId
  }

type PasteId = Int

newtype Content = Content T.Text

data Comment = Comment 
  { _body   :: T.Text
  , _anchor :: Anchor
  }

data Anchor = TopLevel | Line LineNumber

newtype LineNumber = LineNumber Int
