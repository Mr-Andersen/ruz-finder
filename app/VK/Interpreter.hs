module VK.Interpreter where

import GHC.Generics (Generic)
import Control.Monad (unless)

import Data.Text (Text)

import Polysemy
import Polysemy.Error
import Polysemy.Http
import Network.HTTP.Client (BodyReader)

import Data.Aeson


import GHC.Clock (getMonotonicTime)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Float.RealFracMethods
import Control.Concurrent.STM
import Control.Concurrent.STM.Delay

import Numeric.MathFunctions.Constants (m_neg_inf)

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

lastTimeVKRequested :: TMVar Double -- in seconds
lastTimeVKRequested = unsafePerformIO $ newTMVarIO m_neg_inf

vkDelay :: Double -- in seconds
vkDelay = 0.25

-- | Interpreter
-- (HasVK m, VKEndoint endpoint request response) => endpoint -> request -> m (Either Text response)
runVK :: '[ Embed IO, Http BodyReader, Error Text ] `Members` r => VKConfig -> InterpreterFor VK r
runVK config = interpret \(ReqVK e reqData) -> do
    let opts = requestToOption e reqData

    lastTime <- embed $ atomically do
        takeTMVar lastTimeVKRequested
    currTime <- embed getMonotonicTime
    delay <- embed $ newDelay $ ceilingDoubleInt $ (vkDelay + lastTime - currTime) * 1000000
    embed $ atomically do
        waitDelay delay

    reqResult <- getJSON
        "api.vk.com"
        (Path $ "method" <> path e)
        (("access_token", Just $ QueryValue $ _accessToken config)
            : ("v", Just "5.131")
            : opts)

    lastTime' <- embed getMonotonicTime
    embed $ (atomically $ tryPutTMVar lastTimeVKRequested lastTime') >>= flip unless do
        putStrLn "WARNING: tryPutTMVar returned False"

    case reqResult of
        VKROk respOk -> pure respOk
        VKRErr (VkResponseError _ msg) -> throw msg

runVK' :: '[ Embed IO, Http BodyReader, Error Text ] `Members` r => Text -> InterpreterFor VK r
runVK' = runVK . VKC
