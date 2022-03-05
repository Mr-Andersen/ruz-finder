module VK.Find where

import Control.Monad.IO.Class
import Data.Traversable (for)
import Data.Functor
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.Except

import Data.Set (Set)
import Data.Set qualified as Set

import Control.Monad.Memo

import System.Console.AsciiProgress
import Network.HTTP.Req (Req)

import Consts
import VK
import Utils

data VKAccMatch = MatchUni UserId
                | GroupsMatch UserId [(Text, GroupId)]

matchId :: VKAccMatch -> UserId
matchId (MatchUni uid) = uid
matchId (GroupsMatch uid _) = uid

vkIdToText :: UserId -> Text
vkIdToText (UserId uid) = "https://vk.com/id" <> T.pack (show uid)

vkAccMatchToCsv :: VKAccMatch -> Text
vkAccMatchToCsv (MatchUni uid) = vkIdToText uid <> " (совпал университет)"
vkAccMatchToCsv (GroupsMatch uid grps) = vkIdToText uid <> " (совпало групп: " <> T.pack (show $ length grps) <> ")"

findHseGroupsIn :: Set GroupId -> [(Text, GroupId)]
findHseGroupsIn grps = filter (\(_, gid) -> gid `Set.member` grps) hseGroups

getSubscriptionsScript :: [UserId] -> Text
getSubscriptionsScript uids =
    "return [" <> T.intercalate ","
                    ((\(UserId uid) ->
                        "API.users.getSubscriptions({\"user_id\":" <> T.pack (show uid) <> "}).groups.items")
                     <$> uids) <> "];"

findVKAccount :: MonadIO io => Text -> Text -> io [VKAccMatch]
findVKAccount accessToken fullName = runVKTIO accessToken $ runExceptTReporting @(VKT Req) [] do
    usrsRes <- usersSearch (UsersSearchRequest (Just fullName) Nothing) :: ExceptT Text (VKT Req) (Either Text [User])
    -- Пользователи ВК, подходящие по ФИО к fullName
    usrs <- case usrsRes of
        Left err -> throwError err
        Right ok -> pure ok
    -- Пользователи с университетом = ВШЭ
    let usrsWithUni =
            filter (\u -> hseUniId `elem` (universityId <$> fromMaybe [] (universities u))) usrs
    -- Пользователи без указанного у.
    let usrsNoUni =
            filter (null . universities) usrs
    usrsWithGrps <- catMaybes <$> if null usrsNoUni then pure [] else do
        pb <- liftIO $ newProgressBar def { pgFormat = "Getting subs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                          , pgTotal = toInteger (length usrsNoUni)
                                          }
        -- Блоки id по 25 штук
        let uidChunks = chunksOf 25 (userId <$> usrsNoUni)
        -- Пары пользователь-группы (если есть)
        (pairs :: [(UserId, Set GroupId)]) <- concat <$> for uidChunks \uidChunk -> do
            let script = getSubscriptionsScript uidChunk
            (execRes :: Either Text [Maybe [GroupId]]) <- executeCode script <* liftIO (tickN pb (length uidChunk))
            case execRes of
                Left err -> throwError err
                Right gids' -> let (gids :: [Set GroupId]) = maybe mempty Set.fromList <$> gids'
                                in pure (zip uidChunk gids)
        pure $ pairs <&> \(uid, subs) ->
                           if length (findHseGroupsIn subs) > 0
                               then Just (uid, findHseGroupsIn subs)
                               else Nothing
    pure $ fmap (MatchUni . userId) usrsWithUni
            ++ fmap (\(uid, grps) -> GroupsMatch uid grps) usrsWithGrps

findInVKMany :: MonadIO io => Text -> [Text] -> io [(Text, [VKAccMatch])]
findInVKMany accessToken people = do
    pb <- liftIO $ newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                      , pgTotal = toInteger (length people)
                                      }
    startEvalMemoT $ for people \person ->
        (person,) <$> memo (findVKAccount accessToken) person <* liftIO (tick pb)

personMatchToCsv :: (Text, [VKAccMatch]) -> Text
personMatchToCsv (person, mchs) = person <> "," <> T.intercalate "," (vkAccMatchToCsv <$> mchs)
