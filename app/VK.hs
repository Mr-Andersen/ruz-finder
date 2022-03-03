{-# LANGUAGE DuplicateRecordFields, ConstraintKinds #-}

module VK where

import Prelude hiding (id)
import Prelude qualified

import Control.Category ((>>>))
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
-- import Data.Proxy

import Control.Monad.Reader

import Data.Text (Text)
import Data.Text qualified as T
-- import Data.Text.IO qualified as T

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (parseEither)

import Control.Concurrent (threadDelay)
import Network.HTTP.Req

-- import Debug.Trace
-- import Generics.Deriving.Show

data VKConfig = VKC
    { _accessToken :: Text }

type VKT m = ReaderT VKConfig m

type HasVK m = (MonadReader VKConfig m, MonadHttp m)

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
    maxCount :: endpoint -> Maybe Integer

-- reqVK :: FromJSON (w a) => Text -> Option 'Https -> VKT m (w a)
reqVK :: (HasVK m, VKEndoint endpoint request response) => endpoint -> request -> m (Either Text response)
reqVK e reqData = do
    config <- ask
    let opt = requestToOption e reqData
    reqResult <- req GET (https "api.vk.com" /: "method" /: path e)
        NoReqBody
        jsonResponse
        ((opt <> "access_token" =: _accessToken config
              <> "v" =: ("5.131" :: Text))
            & maybe Prelude.id ((<>) . ("count" =:)) (maxCount e))
        <&> responseBody
    liftIO (threadDelay (300 * 1000))
    pure case reqResult of
        VKROk respOk -> Right respOk
        VKRErr (VkResponseError _ msg) -> Left msg

runVKTIO :: MonadIO io => Text -> VKT Req a -> io a
runVKTIO accessToken = flip runReaderT (VKC accessToken)
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

data University = University
    { id :: UniversityId }
    deriving (Generic, Show)

universityId :: University -> UniversityId
universityId (University uid) = uid

instance FromJSON University

data User = User
    { id :: UserId
    , first_name :: Text
    , last_name :: Text
    , universities :: Maybe [University] }
    deriving (Generic, Show)

userId :: User -> UserId
userId (User uid _ _ _) = uid

instance FromJSON User

data UsersSearch = UsersSearch
instance VKEndoint UsersSearch UsersSearchRequest (VKPage User) where
    path UsersSearch = "users.search"
    requestToOption UsersSearch request =
        ("fields" =: ("universities" :: Text))
            & maybe Prelude.id ((<>) . ("q" =:)) (_q request)
            & maybe Prelude.id ((<>) . ("university" =:) . coerce @UniversityId @Integer) (_university request)
    maxCount UsersSearch = Just 1000

usersSearch :: HasVK m => UsersSearchRequest -> m (Either Text [User])
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
    maxCount GetSubscriptions = Just 200

getSubscriptions :: HasVK m => GetSubscriptionsRequest -> m (Either Text [GroupId])
getSubscriptions = fmap (fmap (items . groups)) . reqVK GetSubscriptions

-- ^ users.getSubscriptions

data ExecuteRequest = ExecuteRequest
    { code :: Text }
    deriving (Generic)

data Execute = Execute
instance VKEndoint Execute ExecuteRequest Value where
    path Execute = "execute"
    requestToOption Execute (ExecuteRequest c) = "code" =: c
    maxCount Execute = Nothing

executeCode :: (HasVK m, FromJSON response) => Text -> m (Either Text response)
executeCode c = do
    -- liftIO $ T.putStrLn c
    resp <- reqVK Execute (ExecuteRequest c)
    -- liftIO $ print resp
    pure case parseEither parseJSON <$> resp of
        Right (Right ok) -> Right ok
        Right (Left err) -> Left (T.pack err)
        Left err -> Left err

-- ^ execute
