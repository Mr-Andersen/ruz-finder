module VK ( VK
          , VKEndoint(..)
          , reqVK
          -- ^ VK.Core
          , VKPage(..)
          , UserId(..)
          , GroupId(..)
          , UniversityId(..)
          , UsersSearchRequest(..)
          , University(University)
          , universityId
          , User(..)
          , userId
          , UsersSearch(..)
          , usersSearch
          , GetSubscriptionsRequest(..)
          , GetSubscriptionsResponse(..)
          , GetSubscriptions(..)
          , getSubscriptions
          , ExecuteRequest(..)
          , Execute(..)
          , executeCode ) where

import Prelude hiding (id)
import Prelude qualified

import Control.Category ((>>>))
import Data.Function ((&))

import Data.Text (Text)
import Data.Text qualified as T

import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Http (QueryValue(..))

import Data.Aeson
import Data.Aeson.Types (parseEither)

import GHC.Generics (Generic)

import VK.Core

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
        [("fields", Just "universities")]
            & (_q request
                & maybe Prelude.id (QueryValue >>> Just >>> ("q",) >>> (:)))
            & (_university request
                & maybe Prelude.id (
                    show >>> T.pack
                    >>> QueryValue >>> Just
                    >>> ("university",) >>> (:)))
    maxCount UsersSearch = Just 1000

usersSearch :: VK `Member` r => UsersSearchRequest -> Sem r [User]
usersSearch = fmap items . reqVK UsersSearch

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
    requestToOption GetSubscriptions (GetSubscriptionsRequest (UserId uid)) =
        [("user_id", uid & show & T.pack& QueryValue & Just )]
    maxCount GetSubscriptions = Just 200

getSubscriptions :: VK `Member` r => GetSubscriptionsRequest -> Sem r [GroupId]
getSubscriptions = fmap (items . groups) . reqVK GetSubscriptions

-- ^ users.getSubscriptions

data ExecuteRequest = ExecuteRequest
    { code :: Text }
    deriving (Generic)

data Execute = Execute
instance VKEndoint Execute ExecuteRequest Value where
    path Execute = "execute"
    requestToOption Execute (ExecuteRequest c) =
        [("code", c & QueryValue & Just)]
    maxCount Execute = Nothing

executeCode :: ('[ VK, Error Text ] `Members` r, FromJSON response) => Text -> Sem r response
executeCode c = do
    -- liftIO $ T.putStrLn c
    resp <- reqVK Execute (ExecuteRequest c)
    -- liftIO $ print resp
    case parseEither parseJSON resp of
        Right ok -> pure ok
        Left err -> throw (T.pack err)

-- ^ execute
