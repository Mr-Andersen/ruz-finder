module Main where

import Prelude

import Control.Monad.IO.Class
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Data.Function ((&))
import Data.Functor
import Data.List (permutations)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Data.Text.Lazy.IO qualified as T.Lazy

import System.Environment

import Control.Monad.Except

import Data.Set (Set)
import Data.Set qualified as Set

import Text.HTML.Scalpel

import Control.Monad.Memo
import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress
import Network.HTTP.Req (Req)

import Consts
import OVD.Types
import OVD.HTML
import QueryPerson
import Ruz
import VK hiding (id)

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

runExceptTReporting :: MonadIO io => a -> ExceptT Text io a -> io a
runExceptTReporting defVal act = runExceptT act >>=
    \case
        Left err -> liftIO (T.putStrLn ("ERROR: " <> err)) $> defVal
        Right ok -> pure ok

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

-- Takes ФИО and returns ФИ
dropPatronymic :: Text -> Text
dropPatronymic = T.unwords . take 2 . T.words

withVK :: Text -> [PerPerson Text] -> IO [PerPerson Text]
withVK accessToken people = if null people then pure [] else do
    pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length people)
                             }
    startEvalMemoT $
        for people \(PerPerson (Person fullName' info) _ city loc) ->
            -- Most VK users have only surname and name
            let fullName = dropPatronymic fullName'
             in (memo (findVKAccount accessToken) fullName <* liftIO (tick pb)) <&>
                    (\uids -> PerPerson (Person fullName' info) uids city loc) . fmap matchId

noTokenInstruction :: String
noTokenInstruction =
    "Нет токена ВК. Перейдите по ссылке https://oauth.vk.com/authorize?client_id=8091308&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=friends&response_type=token&v=5.131 "
        ++ "Из адресной строки скопируйте access_token, затем export VK_TOKEN=$access_token"

getAccessToken :: IO Text
getAccessToken = lookupEnv "VK_TOKEN" <&> maybe (Prelude.error noTokenInstruction) T.pack

fromOVD :: String -> IO ()
fromOVD ovdUrl = do
    dataDir <- lookupEnv "DATA_DIR" <&> maybe Prelude.id (<>)
    !accessToken <- getAccessToken
    let allCsv = dataDir "all.csv"
    let freshCsv = dataDir "fresh.csv"

    (oldNames :: [Text]) <- T.readFile allCsv <&> T.lines -- <&> Set.fromList
    let (oldNamesWords :: [Set Text]) = oldNames <&> T.words <&> Set.fromList
    parsed <- fromMaybe [] <$> scrapeURL ovdUrl byCityInBody
    -- parsed <- T.readFile "raw.html" >>= flip scrapeStringLikeT byCityInBody <&> fromMaybe []
    let flattened = parsed
                  & flattenByCity
                  & trimFullNames
                  & filter \perPerson ->
                            let fullNameWords = perPerson & perPersonFullName & T.words & Set.fromList
                             in not (any (Set.isSubsetOf fullNameWords) oldNamesWords)
    (noRuz, fromRuz) <- displayConsoleRegions $ withRuz flattened
    final <- displayConsoleRegions $ withVK accessToken fromRuz
    let freshContent = T.Lazy.unlines $ T.Lazy.fromStrict . perPersonToCsv <$> final
    let allContent = T.Lazy.unlines $ T.Lazy.fromStrict <$>
            Set.toList (Set.fromList (fmap (dropPatronymic . perPersonFullName) final ++ noRuz))
    T.Lazy.appendFile allCsv allContent
    T.Lazy.writeFile freshCsv freshContent

findInVKFromStdin :: IO [(Text, [VKAccMatch])]
findInVKFromStdin = do
    lns <- getContents <&> lines <&> fmap T.pack
    for lns (\person -> (person,) <$> findInVK person)

personMatchToCsv :: (Text, [VKAccMatch]) -> Text
personMatchToCsv (person, mchs) = person <> "," <> T.intercalate "," (vkAccMatchToCsv <$> mchs)

findInVK :: Text -> IO [VKAccMatch]
findInVK fullName = do
    !accessToken <- getAccessToken
    displayConsoleRegions $ findVKAccount accessToken fullName

usage :: String
usage = unlines [ "Usage: ruz-finder COMMAND"
                , "COMMAND = ovd URL | find_in_vk FULL_NAME | find_in_vk - OUT_FILE"
                ]

main :: IO ()
main = getArgs >>=
    \case
        [] -> Prelude.error usage
        ["ovd"] -> Prelude.error "missing URL"
        ["ovd", ovdUrl] -> fromOVD ovdUrl
        ["find_in_vk"] -> Prelude.error "missing FULL_NAME"
        ["find_in_vk", "-"] -> Prelude.error "missing OUT_FILE"
        ["find_in_vk", "-", outFile] ->
            findInVKFromStdin >>= T.writeFile outFile . T.unlines . fmap personMatchToCsv
        ["find_in_vk", fullName] -> findInVK (T.pack fullName) >>= traverse_ (T.putStrLn . vkAccMatchToCsv)
        command : _ -> Prelude.error $ "command " <> command <> " unknown"
