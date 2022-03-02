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

import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress

import Control.Monad.Memo

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

findVKAccount :: MonadIO io => Text -> io [VKAccMatch]
findVKAccount fullName = runVKTIO accessToken do
    usrsRes <- usersSearch (UsersSearchRequest (Just fullName) Nothing)
    usrs <- case usrsRes of
        Right ok -> pure ok
        Left err -> Prelude.error (T.unpack err)
    let usrsWithUni =
            filter (\u -> hseUniId `elem` (universityId <$> fromMaybe [] (universities u))) usrs
    let usrsNoUni =
            filter (null . universities) usrs
    usrsWithGrps <- if null usrsNoUni then pure [] else do
        pb <- liftIO $ newProgressBar def { pgFormat = "Getting subs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                          , pgTotal = toInteger (length usrsNoUni)
                                          }
        concat <$> for (chunksOf 25 (userId <$> usrsNoUni)) \uidChunk -> do
            let script = getSubscriptionsScript uidChunk
            (execRes :: Either Text [Maybe [GroupId]]) <- executeCode script
            liftIO $ tickN pb (length uidChunk)
            catMaybes <$> case (zip uidChunk . fmap Set.fromList . catMaybes) <$> execRes of
                Right pairs -> pure $ pairs <&> \(uid, subs) ->
                                        if length (findHseGroupsIn subs) > 0
                                            then Just (uid, findHseGroupsIn subs)
                                            else Nothing
                Left err -> Prelude.error $ T.unpack err
    pure $ fmap (MatchUni . userId) usrsWithUni
            ++ fmap (\(uid, grps) -> GroupsMatch uid grps) usrsWithGrps

withVK :: [PerPerson Text] -> IO [PerPerson Text]
withVK people = if null people then pure [] else do
    pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length people)
                             }
    startEvalMemoT $
        for people \(PerPerson (Person fullName' info) _ city loc) ->
            -- Most VK users have only surname and name
            let fullName = fullName' & T.words & take 2 & T.unwords
             in (memo findVKAccount fullName <* liftIO (tick pb)) <&>
                    (\uids -> PerPerson (Person fullName info) uids city loc) . fmap matchId

main :: IO ()
main = displayConsoleRegions do
    (oldNames :: [Text]) <- T.readFile "all.csv" <&> T.lines -- <&> Set.fromList
    rawHtml <- T.readFile "raw.html"
    flattened <- parseByCity rawHtml
                <&> flattenByCity
                <&> trimFullNames
                <&> filter (\(PerPerson (Person n _) _ _ _) ->
                        not (any (Set.isSubsetOf (Set.fromList (T.words n)) . Set.fromList . T.words) oldNames))
    (noRuz, fromRuz) <- withRuz flattened
    final <- fromRuz & withVK
    let freshContent = T.Lazy.unlines $ T.Lazy.fromStrict . perPersonToCsv <$> final
    let allContent = T.Lazy.unlines $ T.Lazy.fromStrict <$>
            Set.toList (Set.fromList (fmap perPersonFullName final ++ noRuz))
    T.Lazy.writeFile "fresh.csv" freshContent
    T.Lazy.appendFile "all.csv" allContent
