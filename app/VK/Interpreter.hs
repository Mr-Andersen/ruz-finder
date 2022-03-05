module VK.Interpreter where

import GHC.Generics (Generic)
import Control.Concurrent (threadDelay)

import Data.Text (Text)

import Polysemy
import Polysemy.Error
import Polysemy.Http
import Network.HTTP.Client (BodyReader)

import Data.Aeson

import VK.Core
import Utils

data VKConfig = VKC
    { _accessToken :: Text }

data VkResponseError = VkResponseError
    { error_code :: Integer
    , error_msg :: Text }
    deriving (Generic)

instance FromJSON VkResponseError

data VKResponse a = VKROk { response :: a }
                  | VKRErr { error :: VkResponseError }
    deriving (Generic)

instance FromJSON a => FromJSON (VKResponse a) where
    parseJSON = genericParseJSON defaultOptions { sumEncoding = UntaggedValue }

-- | Interpreter
-- (HasVK m, VKEndoint endpoint request response) => endpoint -> request -> m (Either Text response)
runVK :: '[ Embed IO, Http BodyReader, Error Text ] `Members` r => VKConfig -> InterpreterFor VK r
runVK config = interpret \(ReqVK e reqData) -> do
    let opts = requestToOption e reqData
    reqResult <- getJSON
        "api.vk.com"
        (Path $ "method" <> path e)
        (("access_token", Just $ QueryValue $ _accessToken config)
            : ("v", Just "5.131")
            : opts)
    embed (threadDelay (300 * 1000))
    case reqResult of
        VKROk respOk -> pure respOk
        VKRErr (VkResponseError _ msg) -> throw msg

runVK' :: '[ Embed IO, Http BodyReader, Error Text ] `Members` r => Text -> InterpreterFor VK r
runVK' = runVK . VKC
