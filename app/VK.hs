module VK where

import Prelude hiding (id)
import Prelude qualified

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
-- import Data.Proxy

import Data.Text (Text)
-- import Data.Text qualified as T
-- import Data.Text.IO qualified as T

import GHC.Generics (Generic)
import Data.Aeson

import Control.Concurrent (threadDelay)
import Network.HTTP.Req

-- import Debug.Trace
-- import Generics.Deriving.Show

data VKConfig = VKC
    { _accessToken :: Text }

newtype VKT m a = VKT
    { runVKT :: VKConfig -> m a }
    deriving (Functor)

instance Applicative m => Applicative (VKT m) where
    pure = VKT . const . pure
    VKT f <*> VKT x = VKT \c -> f c <*> x c

instance Monad m => Monad (VKT m) where
    VKT x >>= f = VKT \c -> let x' = x c
                                f' = flip (runVKT . f) c
                             in x' >>= f'

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

class FromJSON response => VKEndoint endpoint request response | endpoint -> request
                                                               , endpoint -> response where
    path :: endpoint -> Text
    requestToOption :: endpoint -> request -> Option 'Https
    maxCount :: endpoint -> Integer

-- reqVK :: FromJSON (w a) => Text -> Option 'Https -> VKT m (w a)
reqVK :: MonadHttp m => VKEndoint endpoint request response => endpoint -> request -> VKT m (Either Text response)
reqVK e reqData = VKT $ \config -> do
    let opt = requestToOption e reqData
    reqResult <- req GET (https "api.vk.com" /: "method" /: path e)
        NoReqBody
        jsonResponse
        (opt <> "access_token" =: _accessToken config
             <> "v" =: ("5.131" :: Text)
             <> "count" =: maxCount e)
        <&> responseBody
    liftIO (threadDelay (200 * 1000))
    pure case reqResult of
        VKROk respOk -> Right respOk
        VKRErr (VkResponseError _ msg) -> Left msg

runVKTIO :: Text -> VKT Req a -> IO a
runVKTIO accessToken = runVKT
                       >>> ($ VKC accessToken)
                       >>> runReq defaultHttpConfig

-- ^ Generic API

data VKPage a = VKPage
    { count :: Integer
    , items :: [a] }
    deriving (Generic)

instance FromJSON a => FromJSON (VKPage a)

newtype UserId = UserId Integer deriving (Generic, Show, Eq, Ord)
instance FromJSON UserId

newtype GroupId = GroupId Integer deriving (Generic, Show, Eq, Ord)
instance FromJSON GroupId

newtype UniversityId = UniversityId Integer deriving (Generic, Show, Eq, Ord)
instance FromJSON UniversityId

-- ^ Utils

data UsersSearchRequest = UsersSearchRequest
    { _q :: Maybe Text
    -- , fields :: Maybe [Text]
    , _university :: Maybe UniversityId }

data User = User
    { id :: UserId
    , first_name :: Text
    , last_name :: Text }
    deriving (Generic, Show)

instance FromJSON User

data UsersSearch = UsersSearch
instance VKEndoint UsersSearch UsersSearchRequest (VKPage User) where
    path UsersSearch = "users.search"
    requestToOption UsersSearch request =
        mempty & maybe Prelude.id ((<>) . ("q" =:)) (_q request)
               & maybe Prelude.id ((<>) . ("university" =:) . coerce @UniversityId @Integer) (_university request)
    maxCount UsersSearch = 1000

usersSearch :: MonadHttp m => UsersSearchRequest -> VKT m (Either Text [User])
usersSearch = fmap (fmap items) . reqVK UsersSearch

-- ^ users.search

newtype GetSubscriptionsRequest = GetSubscriptionsRequest
    { _userId :: UserId }

data GetSubscriptionsResponse = GetSubscriptionsResponse
    { users :: VKPage UserId
    , groups :: VKPage GroupId }
    deriving (Generic)

instance FromJSON GetSubscriptionsResponse

data GetSubscriptions = GetSubscriptions
instance VKEndoint GetSubscriptions GetSubscriptionsRequest GetSubscriptionsResponse where
    path GetSubscriptions = "users.getSubscriptions"
    requestToOption GetSubscriptions (GetSubscriptionsRequest (UserId uid)) = "user_id" =: uid
    maxCount GetSubscriptions = 200

getSubscriptions :: MonadHttp m => GetSubscriptionsRequest -> VKT m (Either Text [GroupId])
getSubscriptions = fmap (fmap (items . groups)) . reqVK GetSubscriptions

-- ^ users.getSubscriptions
