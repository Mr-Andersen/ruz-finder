module VK.Find where

import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.List.Split (chunksOf)
import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T

import Polysemy
import Polysemy.Cache
import Polysemy.Error

import Data.Set (Set)
import Data.Set qualified as Set

import System.Console.AsciiProgress

import Consts
import VK

findHseGroupsIn :: Set GroupId -> [(Text, GroupId)]
findHseGroupsIn grps = filter (\(_, gid) -> gid `Set.member` grps) hseGroups

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

getSubscriptionsScript :: [UserId] -> Text
getSubscriptionsScript uids =
    "return [" <> T.intercalate ","
                    ((\(UserId uid) ->
                        "API.users.getSubscriptions({\"user_id\":" <> T.pack (show uid) <> "}).groups.items")
                     <$> uids) <> "];"

findVKAccount :: '[ Embed IO, VK, Error Text ] `Members` r
              => Text
              -> Sem r [VKAccMatch]
findVKAccount fullName = do
    -- Пользователи ВК, подходящие по ФИО к fullName
    usrs <- usersSearch (UsersSearchRequest (Just fullName) Nothing)
    -- Пользователи с университетом = ВШЭ
    let usrsWithUni =
            filter (\u -> (`Set.member` hseUniIds) `any` (universityId <$> fromMaybe [] (universities u))) usrs
    -- Пользователи без указанного у.
    let usrsNoUni =
            filter (maybe False null . universities) usrs
    usrsWithGrps <- catMaybes <$> if null usrsNoUni then pure [] else do
        pb <- embed $ newProgressBar def { pgFormat = "Getting subs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                         , pgTotal = toInteger (length usrsNoUni)
                                         }
        -- Блоки id по 25 штук
        let uidChunks = chunksOf 25 (userId <$> usrsNoUni)
        -- Пары пользователь-группы (если есть)
        (pairs :: [(UserId, Set GroupId)]) <- concat <$> for uidChunks \uidChunk -> do
            let script = getSubscriptionsScript uidChunk
            (gids' :: [Maybe [GroupId]]) <- executeCode script <* embed (tickN pb (length uidChunk))
            let (gids :: [Set GroupId]) = maybe mempty Set.fromList <$> gids'
            pure (zip uidChunk gids)
        pure $ pairs <&> \(uid, subs) ->
                           if length (findHseGroupsIn subs) > 0
                               then Just (uid, findHseGroupsIn subs)
                               else Nothing
    pure $ fmap (MatchUni . userId) usrsWithUni
            ++ fmap (\(uid, grps) -> GroupsMatch uid grps) usrsWithGrps

findInVKMany :: '[ Embed IO, VK, Error Text ] `Members` r
             => [Text] -> Sem r [[VKAccMatch]]
findInVKMany people = do
    pb <- embed $ newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                     , pgTotal = toInteger (length people)
                                     }
    runCache' @Text @[VKAccMatch] $ for people \person ->
        memo findVKAccount person <* embed (tick pb)
