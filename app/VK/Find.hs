module VK.Find where

import Consts
import Control.Concurrent.Async.Pool
import Control.Monad.Except
import Data.Functor
import Data.List (partition)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Network.HTTP.Req (Req)

-- import System.Console.AsciiProgress
import Utils
import VK

data VKAccMatch
  = MatchUni UserId
  | GroupsMatch UserId [(Text, GroupId)]

matchId :: VKAccMatch -> UserId
matchId (MatchUni uid) = uid
matchId (GroupsMatch uid _) = uid

vkIdToText :: UserId -> Text
vkIdToText (UserId uid) = "https://vk.com/id" <> T.pack (show uid)

vkAccMatchToCsv :: VKAccMatch -> Text
vkAccMatchToCsv (MatchUni uid) = vkIdToText uid <> " (совпал университет)"
vkAccMatchToCsv (GroupsMatch uid grps) =
  vkIdToText uid <> " (совпало групп " <> T.pack (show $ length grps) <> ": " <> T.unwords (fst <$> grps) <> ")"

findHseGroupsIn :: Set GroupId -> [(Text, GroupId)]
findHseGroupsIn grps = filter (\(_, gid) -> gid `Set.member` grps) hseGroups

getSubscriptionsScript :: [UserId] -> Text
getSubscriptionsScript uids =
  "return ["
    <> T.intercalate
      ","
      ( ( \(UserId uid) ->
            "API.users.getSubscriptions({\"user_id\":" <> T.pack (show uid) <> "}).groups.items"
        )
          <$> uids
      )
    <> "];"

findVKAccount :: MonadIO io => [Text] -> Text -> io (Either Text [VKAccMatch])
findVKAccount accessTokens fullName = runVKTIO accessTokens $ runExceptTReporting @(VKT Req) do
  usrs :: [User] <- retry 5 $ liftEither =<< usersSearch (UsersSearchRequest (Just fullName) Nothing)
  -- Пользователи ВК, подходящие по ФИО к fullName
  -- usrs <- case usrsRes of
  --     Left err -> throwError err
  --     Right ok -> pure ok
  let (usrsWithUni, usrsNoUni) =
        partition (\u -> (`Set.member` hseUniIds) `any` (universityId <$> fromMaybe [] (universities u))) usrs
  -- -- Пользователи с университетом = ВШЭ
  -- let usrsWithUni =
  --         filter (\u -> hseUniId `elem` (universityId <$> fromMaybe [] (universities u))) usrs
  -- -- Пользователи без указанного у.
  -- let usrsNoUni =
  --         filter (maybe True null . universities) usrs
  usrsWithGrps <-
    catMaybes
      <$> if null usrsNoUni
        then pure []
        else do
          -- pb <- liftIO $ newProgressBar def { pgFormat = "Getting subs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
          --                                   , pgTotal = toInteger (length usrsNoUni)
          --                                   }
          -- Блоки id по 25 штук
          let uidChunks = chunksOf 25 (userId <$> usrsNoUni)
          -- Пары пользователь-группы (если есть)
          (pairs :: [(UserId, Set GroupId)]) <-
            concat <$> for uidChunks \uidChunk -> do
              let script = getSubscriptionsScript uidChunk
              gids' :: [Maybe [GroupId]] <- retry 5 $ liftEither =<< executeCode script
              let (gids :: [Set GroupId]) = maybe mempty Set.fromList <$> gids'
              pure (zip uidChunk gids)
          pure $
            pairs <&> \(uid, subs) ->
              let hseSubs = findHseGroupsIn subs
               in if length hseSubs > 0
                    then Just $ GroupsMatch uid hseSubs
                    else Nothing
  pure $
    fmap (MatchUni . userId) usrsWithUni
      ++ usrsWithGrps

findInVKMany :: MonadIO io => [Text] -> [Text] -> io [(Text, (Either Text [VKAccMatch]))]
findInVKMany accessTokens people = liftIO do
  -- pb <- liftIO $ newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
  --                                   , pgTotal = toInteger (length people)
  --                                   }
  withTaskGroup (length accessTokens) \tg -> do
    forConcurrently tg people \person ->
      (person,) <$> findVKAccount accessTokens person -- <* liftIO (tick pb)

personMatchToCsv :: (Text, [VKAccMatch]) -> Text
personMatchToCsv (person, mchs) = person <> "," <> T.intercalate "," (vkAccMatchToCsv <$> mchs)
