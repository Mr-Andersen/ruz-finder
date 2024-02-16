{-# LANGUAGE ConstraintKinds #-}

module VK where

import Prelude hiding (id)
import Prelude qualified

import Control.Category ((>>>))
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Coerce (coerce)
import Data.Foldable (minimumBy)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (threadDelay)
import GHC.Generics (Generic)
import Network.HTTP.Req
import System.IO.Unsafe (unsafePerformIO)
import Utils

data VKConfig = VKC
  { _accessTokens :: [Text]
  -- ^ In order of preference
  }

type VKT m = ReaderT VKConfig m

type HasVK m = (MonadReader VKConfig m, MonadHttp m, MonadIO m)

data VkResponseError = VkResponseError
  { error_code :: Integer
  , error_msg :: Text
  }
  deriving (Generic)

instance FromJSON VkResponseError

data VKResponse a
  = VKROk {response :: a}
  | VKRErr {error :: VkResponseError}
  deriving (Generic)

instance FromJSON a => FromJSON (VKResponse a) where
  parseJSON = genericParseJSON defaultOptions {sumEncoding = UntaggedValue}

class
  (FromJSON response, Show request) =>
  VKEndoint endpoint request response
    | endpoint -> request
    , endpoint -> response
  where
  path :: endpoint -> Text
  requestToOption :: endpoint -> request -> Option 'Https
  maxCount :: endpoint -> Maybe Integer

lastTimeVKRequested :: MVar (Map Text Word64) -- Map (VKToken -> MicroSeconds)
lastTimeVKRequested = unsafePerformIO $ newMVar mempty

vkDelay :: Word64 -- in microseconds
vkDelay = 350_000

reqVK :: (HasVK m, VKEndoint endpoint request response) => endpoint -> request -> m (Either Text response)
reqVK e reqData = do
  config <- ask
  let accessTokens = _accessTokens config
  let opt = requestToOption e reqData

  !(toWait, accessToken) <- liftIO $ modifyMVar lastTimeVKRequested \lastTimeVKRequested' -> do
    !currentTime <- getMonotonicTimeNSec <&> (`div` 1000)
    let (accessToken, lastTimeOpt) =
          accessTokens
            & fmap (\at -> (at, Map.lookup at lastTimeVKRequested'))
            & minimumBy (comparing snd)
        toWait = case lastTimeOpt of
          Nothing -> 0
          Just lastTime ->
            if vkDelay + lastTime > currentTime
              then vkDelay + lastTime - currentTime
              else 0
    ePutTextLn $ "token = " <> accessToken
    ePutTextLn $ "toWait = " <> showText toWait
    pure
      ( lastTimeVKRequested' & Map.insert accessToken (currentTime + toWait)
      , (toWait, accessToken)
      )

  when (toWait > 0) do
    liftIO $ threadDelay $ fromEnum $ toWait

  reqResult <-
    req
      GET
      (https "api.vk.com" /: "method" /: path e)
      NoReqBody
      jsonResponse
      ( ( opt
            <> "access_token" =: accessToken
            <> "v" =: ("5.131" :: Text)
        )
          & maybe Prelude.id ((<>) . ("count" =:)) (maxCount e)
      )
      <&> responseBody

  pure case reqResult of
    VKROk respOk -> Right respOk
    VKRErr (VkResponseError _ msg) -> Left msg

runVKTIO :: MonadIO io => [Text] -> VKT Req a -> io a
runVKTIO accessTokens =
  flip runReaderT (VKC accessTokens)
    >>> runReq defaultHttpConfig
-- ^ Generic API

data VKPage a = VKPage
  { count :: Integer
  , items :: [a]
  }
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
  , -- , fields :: Maybe [Text]
    _university :: Maybe UniversityId
  }

instance Show UsersSearchRequest where
  show UsersSearchRequest {_q = q, _university = u} =
    "q = " ++ maybe "null" T.unpack q ++ ", university = " ++ maybe "null" (\(UniversityId i) -> show i) u

data University = University
  {id :: UniversityId}
  deriving (Generic, Show)

universityId :: University -> UniversityId
universityId University {id = uid} = uid

instance FromJSON University

data User = User
  { id :: UserId
  , first_name :: Text
  , last_name :: Text
  , universities :: Maybe [University]
  }
  deriving (Generic, Show)

userId :: User -> UserId
userId (User uid _ _ _) = uid

instance FromJSON User

data UsersSearch = UsersSearch
instance VKEndoint UsersSearch UsersSearchRequest (VKPage User) where
  path UsersSearch = "users.search"
  requestToOption UsersSearch request =
    ("fields" =: ("universities,uni_year" :: Text))
      & maybe Prelude.id ((<>) . ("q" =:)) (_q request)
      & maybe Prelude.id ((<>) . ("university" =:) . coerce @UniversityId @Integer) (_university request)
  maxCount UsersSearch = Just 1000

usersSearch :: HasVK m => UsersSearchRequest -> m (Either Text [User])
usersSearch = fmap (fmap items) . reqVK UsersSearch
-- ^ users.search

newtype GetSubscriptionsRequest = GetSubscriptionsRequest
  {_userId :: UserId}
  deriving (Show)

data GetSubscriptionsResponse = GetSubscriptionsResponse
  { users :: VKPage UserId
  , groups :: VKPage GroupId
  }
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
  {code :: Text}
  deriving (Generic, Show)

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
