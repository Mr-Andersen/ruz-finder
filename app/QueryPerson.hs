module QueryPerson where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data QueryResult = QueryResult
  { label :: Text
  , description :: Text
  }
  deriving (Generic)

instance FromJSON QueryResult

class QueryPerson s where
  query :: MonadIO io => s -> [Text] -> io [QueryResult]
