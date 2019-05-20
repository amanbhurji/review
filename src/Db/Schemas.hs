module Db.Schemas where

import qualified Data.UUID      as DU
import qualified Data.Text      as T
import           GHC.Int

data Paste = Paste
  { p_pid :: DU.UUID
  , p_body :: T.Text
  }

data Comment = Comment
  { c_pid :: DU.UUID
  , c_lno :: Int16
  , c_cno :: Int16
  , c_body :: T.Text
  }
