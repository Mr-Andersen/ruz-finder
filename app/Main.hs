module Main where

import Prelude

import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Data.Function ((&))
import Data.Functor
import Data.Functor.Identity
import Data.List (permutations)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as T.Lazy
import Data.Text.Lazy.IO qualified as T.Lazy

import Polysemy
import Polysemy.Error -- (Error, runError, throw)
import Polysemy.Cache

import System.Environment

import Data.Set (Set)
import Data.Set qualified as Set

import Text.HTML.Scalpel

import System.Console.AsciiProgress

import Control.Concurrent.Async.Pool qualified as A

import Consts
import OVD.Types
import OVD.HTML
import QueryPerson
import Ruz
import VK hiding (id)
import VK.Interpreter
import Utils

permutFullNames :: [PerPerson Text] -> [PerPerson [Text]]
permutFullNames perPerson =
    concat $ perPerson
        <&> \(PerPerson (Person fullName i) uids city loc) -> permutations (T.words fullName)
            <&> \fullNameWords -> PerPerson (Person fullNameWords i) uids city loc

withTaskGroup' :: Int -> (A.TaskGroup -> Sem '[ Embed IO ] a)
               -> Sem '[ Embed IO ] a
withTaskGroup' n h = withTaskGroup (fmap Identity) (pure . runIdentity) n h

mapConcurrently' :: Traversable t
                 => A.TaskGroup -> (a -> Sem '[ Embed IO ] b) -> t a
                 -> Sem '[ Embed IO ] (t b)
mapConcurrently' = mapConcurrently (fmap Identity) (pure . runIdentity)

-- Returns (names not found in Ruz, names with info from Ruz)
withRuz :: Embed IO `Member` r
        => [PerPerson Text]
        -> Sem r ([Text], [PerPerson Text])
withRuz perPerson = embed $ if null perPerson then pure ([], []) else do
    pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length perPerson)
                             }
    runM $ fmap concat . partitionEithers @Text @[PerPerson Text]
        <$> withTaskGroup' 4 \g -> flip (mapConcurrently' g) (permutFullNames perPerson)
            \(PerPerson (Person fullNameWords _) uids city loc) -> interpretHttpIO $ runError do
                ruzResults <- queryPerson Ruz fullNameWords <* embed (tick pb)
                if null ruzResults then
                    throw (T.unwords fullNameWords)
                else pure $ ruzResults
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

findVKAccount :: '[ Embed IO, VK, Error Text ] `Members` r
              => Text
              -> Sem r [VKAccMatch]
findVKAccount fullName = do
    -- Пользователи ВК, подходящие по ФИО к fullName
    usrs <- usersSearch (UsersSearchRequest (Just fullName) Nothing)
    -- Пользователи с университетом = ВШЭ
    let usrsWithUni =
            filter (\u -> hseUniId `elem` (universityId <$> fromMaybe [] (universities u))) usrs
    -- Пользователи без указанного у.
    let usrsNoUni =
            filter (null . universities) usrs
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

-- Takes ФИО and returns ФИ
dropPatronymic :: Text -> Text
dropPatronymic = T.unwords . take 2 . T.words

withVK :: '[ Embed IO, VK, Error Text ] `Members` r
       => [PerPerson Text] -> Sem r [PerPerson Text]
withVK perPersons = do
    let people = dropPatronymic . perPersonFullName <$> perPersons
    mchs' <- findInVKMany people
    pure $ zip perPersons mchs'
            <&> \(PerPerson (Person fullName info) _ city loc, mchs) ->
                  PerPerson (Person fullName info) (matchId <$> mchs) city loc
        -- <&> fmap ((\uids -> PerPerson (Person fullName' info) uids city loc) . fmap matchId)
-- withVK accessToken people = if null people then pure (Right []) else do
--     pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
--                              , pgTotal = toInteger (length people)
--                              }
--     startEvalMemoT $
--         for people \(PerPerson (Person fullName' info) _ city loc) ->
--             -- Most VK users have only surname and name
--             let fullName = dropPatronymic fullName'
--              in (memo (findVKAccount accessToken) fullName <* liftIO (tick pb)) <&>
--                     fmap ((\uids -> PerPerson (Person fullName' info) uids city loc) . fmap matchId)

noTokenInstruction :: Text
noTokenInstruction =
    "Нет токена ВК. Перейдите по ссылке "
        <> "https://oauth.vk.com/authorize?client_id=8091308&display=page&redirect_uri=https://oauth.vk.com/blank.html&scope=friends,groups,offline&response_type=token&v=5.131 "
        <> "Из адресной строки скопируйте access_token, затем export VK_TOKEN=$access_token"

getAccessToken :: '[ Embed IO, Error Text ] `Members` r => Sem r Text
getAccessToken = embed (lookupEnv "VK_TOKEN") >>= maybe (throw noTokenInstruction) (pure . T.pack)

fromOVD :: '[ Embed IO, VK, Error Text ] `Members` r
        => String -> Sem r ()
fromOVD ovdUrl = do
    dataDir <- embed $ lookupEnv "DATA_DIR" <&> maybe Prelude.id (<>)
    -- !accessToken <- getAccessToken
    let allCsv = dataDir "all.csv"
    let freshCsv = dataDir "fresh.csv"

    (oldNames :: [Text]) <- embed $ T.readFile allCsv <&> T.lines -- <&> Set.fromList
    let (oldNamesWords :: [Set Text]) = oldNames <&> T.words <&> Set.fromList
    parsed <- embed $ fromMaybe [] <$> scrapeURL ovdUrl byCityInBody
    -- parsed <- T.readFile "raw.html" >>= flip scrapeStringLikeT byCityInBody <&> fromMaybe []
    let flattened = parsed
                  & flattenByCity
                  & trimFullNames
                  & filter \perPerson ->
                            let fullNameWords = perPerson & perPersonFullName & T.words & Set.fromList
                             in not (any (Set.isSubsetOf fullNameWords) oldNamesWords)
    (noRuz, fromRuz) <- withRuz flattened
    final <- withVK fromRuz
    let freshContent = T.Lazy.unlines $ T.Lazy.fromStrict . perPersonToCsv <$> final
    let allContent = T.Lazy.unlines $ T.Lazy.fromStrict <$>
            Set.toList (Set.fromList (fmap (dropPatronymic . perPersonFullName) final ++ noRuz))
    embed $ T.Lazy.appendFile allCsv allContent
    embed $ T.Lazy.writeFile freshCsv freshContent

findInVKMany :: '[ Embed IO, VK, Error Text ] `Members` r
             => [Text] -> Sem r [[VKAccMatch]]
findInVKMany people = do
    pb <- embed $ newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                     , pgTotal = toInteger (length people)
                                     }
    runCache' @Text @[VKAccMatch] $ for people \person ->
        memo findVKAccount person <* embed (tick pb)

personMatchToCsv :: (Text, [VKAccMatch]) -> Text
personMatchToCsv (person, mchs) = person <> "," <> T.intercalate "," (vkAccMatchToCsv <$> mchs)

usage :: Text
usage = T.unlines [ "Usage: ruz-finder COMMAND"
                  , "COMMAND = ovd URL | find_in_vk FULL_NAME | find_in_vk - OUT_FILE"
                  ]

runVK'' :: '[ Embed IO, Error Text ] `Members` r => InterpreterFor VK r
runVK'' act = do
    accessToken <- getAccessToken
    interpretHttpIO $ runVK' accessToken $ raiseUnder act

main :: IO ()
main = getArgs >>= displayConsoleRegions
                    . runFinal . embedToFinal @IO
                    . (>>= either (embed . T.putStrLn . ("ERROR: " <>)) pure)
                    . runError
    . \case
        [] -> throwT usage
        ["ovd"] -> throwT "missing URL"
        ["ovd", ovdUrl] -> runVK'' $ fromOVD ovdUrl
        ["find_in_vk"] -> throwT "missing FULL_NAME"
        ["find_in_vk", "-"] -> throwT "missing OUT_FILE"
        ["find_in_vk", fullName'] -> runVK'' do
            let fullName = T.pack fullName'
            findInVKMany [fullName]
                >>= traverse_ (embed . T.putStrLn . personMatchToCsv . (fullName,))
        ["find_in_vk", inFile, outFile] -> runVK'' do
            lns <- T.lines <$> case inFile of
                "-" -> embed T.getContents
                inFile' -> embed $ T.readFile inFile'
            matchesCsv <- findInVKMany lns <&> fmap personMatchToCsv . zip lns
            let outContent = T.unlines matchesCsv
            case outFile of
                "-" -> embed $ T.putStrLn outContent
                outFile' -> embed $ T.writeFile outFile' outContent
        command : opts -> throwT $ "command " <> T.pack command
                                              <> " unknown (options: "
                                              <> T.pack (unwords opts)
                                              <> ")"
