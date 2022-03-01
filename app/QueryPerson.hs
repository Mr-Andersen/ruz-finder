module QueryPerson where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data QueryResult = QueryResult
    { label :: Text
    , description :: Text }
    deriving (Generic)

instance FromJSON QueryResult

class QueryPerson s where
    query :: MonadIO io => s -> [Text] -> io [QueryResult]
