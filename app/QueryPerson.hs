module QueryPerson where

import Data.Text (Text)

import Polysemy (Sem, Members)
import Polysemy.Error (Error)
import Polysemy.Http (Http)
import Network.HTTP.Client (BodyReader)

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

data QueryResult = QueryResult
    { label :: Text
    , description :: Text }
    deriving (Generic)

instance FromJSON QueryResult

class QueryPerson s where
    queryPerson :: '[ Http BodyReader, Error Text ] `Members` r => s -> [Text] -> Sem r [QueryResult]
