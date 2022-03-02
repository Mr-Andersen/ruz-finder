module Main where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Either (partitionEithers)
-- import Data.Foldable (for_)
-- import Data.Traversable (for)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (permutations)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Data.Text.Lazy.IO qualified as T.Lazy

import Data.Set (Set)
import Data.Set qualified as Set

import Text.HTML.Scalpel

import Control.Monad.Memo
import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress

import Consts
import OVD.Types
import OVD.HTML
import QueryPerson
import Ruz
import VK

permutFullNames :: [PerPerson Text] -> [PerPerson [Text]]
permutFullNames perPerson =
    concat $ perPerson
        <&> \(PerPerson (Person fullName i) uids city loc) -> permutations (T.words fullName)
            <&> \fullNameWords -> PerPerson (Person fullNameWords i) uids city loc

-- Returns (names not found in Ruz, names with info from Ruz)
withRuz :: MonadIO io => [PerPerson Text] -> io ([Text], [PerPerson Text])
withRuz perPerson = if null perPerson then pure ([], []) else liftIO do
    pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length perPerson)
                             }
    fmap concat . partitionEithers @Text @[PerPerson Text]
        <$> withTaskGroup 4 \g -> flip (mapConcurrently g) (permutFullNames perPerson)
            \(PerPerson (Person fullNameWords _) uids city loc) -> do
                ruzResults <- query Ruz fullNameWords <* tick pb
                pure $ if null ruzResults then
                            Left (T.unwords fullNameWords)
                        else Right $ ruzResults
                                        <&> \(QueryResult lbl descr) ->
                                              PerPerson (Person lbl descr) uids city loc

findHseGroupsIn :: Set GroupId -> [(Text, GroupId)]
findHseGroupsIn grps = filter (\(_, gid) -> gid `Set.member` grps) hseGroups

data VKAccMatch = MatchUni UserId
                | GroupsMatch UserId [(Text, GroupId)]

matchId :: VKAccMatch -> UserId
matchId (MatchUni uid) = uid
matchId (GroupsMatch uid _) = uid

getSubscriptionsScript :: [UserId] -> Text
getSubscriptionsScript uids =
    "return [" <> T.intercalate ","
                    ((\(UserId uid) ->
                        "API.users.getSubscriptions({\"user_id\":" <> T.pack (show uid) <> "}).groups.items")
                     <$> uids) <> "];"

runExceptTReporting :: Monad io => a -> ExceptT Text io a -> io a
runExceptTReporting defVal act = runExceptT >>=
    \case
        Left err -> liftIO (T.putStrLn ("ERROR: " <> err)) $> defVal
        Right ok -> pure ok

findVKAccount :: MonadIO io => Text -> io [VKAccMatch]
findVKAccount fullName = runVKTIO accessToken do
    usrsRes <- usersSearch (UsersSearchRequest (Just fullName) Nothing)
    -- Пользователи ВК, подходящие по ФИО к fullName
    usrs <- case usrsRes of
        Left err -> Prelude.error (T.unpack err)
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
            pure case execRes of
                Left err -> Prelude.error $ T.unpack err
                Right gids' -> let (gids :: [Set GroupId]) = maybe mempty Set.fromList <$> gids'
                                in zip uidChunk gids
        pure $ pairs <&> \(uid, subs) ->
                           if length (findHseGroupsIn subs) > 0
                               then Just (uid, findHseGroupsIn subs)
                               else Nothing
    pure $ fmap (MatchUni . userId) usrsWithUni
            ++ fmap (\(uid, grps) -> GroupsMatch uid grps) usrsWithGrps

-- Takes ФИО and returns ФИ
dropPatronymic :: Text -> Text
dropPatronymic = T.unwords . take 2 . T.words

withVK :: [PerPerson Text] -> IO [PerPerson Text]
withVK people = if null people then pure [] else do
    pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length people)
                             }
    startEvalMemoT $
        for people \(PerPerson (Person fullName' info) _ city loc) ->
            -- Most VK users have only surname and name
            let fullName = dropPatronymic fullName'
             in (memo findVKAccount fullName <* liftIO (tick pb)) <&>
                    (\uids -> PerPerson (Person fullName' info) uids city loc) . fmap matchId

main :: IO ()
main = displayConsoleRegions do
    (oldNames :: [Text]) <- T.readFile "all.csv" <&> T.lines -- <&> Set.fromList
    let (oldNamesWords :: [Set Text]) = oldNames <&> T.words <&> Set.fromList
    parsed <- fromMaybe [] <$>
        scrapeURL
            "https://ovd.news/news/2022/03/02/spiski-zaderzhannyh-v-svyazi-s-akciyami-protiv-voyny-s-ukrainoy-2-marta-2022-goda"
            byCityInBody
    -- parsed <- T.readFile "raw.html" >>= flip scrapeStringLikeT byCityInBody <&> fromMaybe []
    let flattened = parsed
                  & flattenByCity
                  & trimFullNames
                  & filter \perPerson ->
                            let fullNameWords = perPerson & perPersonFullName & T.words & Set.fromList
                             in not (any (Set.isSubsetOf fullNameWords) oldNamesWords)
    (noRuz, fromRuz) <- withRuz flattened
    final <- fromRuz & withVK
    let freshContent = T.Lazy.unlines $ T.Lazy.fromStrict . perPersonToCsv <$> final
    let allContent = T.Lazy.unlines $ T.Lazy.fromStrict <$>
            Set.toList (Set.fromList (fmap (dropPatronymic . perPersonFullName) final ++ noRuz))
    T.Lazy.appendFile "all.csv" allContent
    T.Lazy.writeFile "fresh.csv" freshContent
