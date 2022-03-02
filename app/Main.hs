module Main where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Data.Foldable (for_)
-- import Data.Traversable (for)
-- import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (permutations)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.Set (Set)
import Data.Set qualified as Set

import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress

import Control.Monad.Memo

import OVD.Types
import OVD.HTML
import QueryPerson
import Ruz
import VK

permutFullNames :: [PerPerson Text] -> [PerPerson [Text]]
permutFullNames perPerson =
    concat $ perPerson
        <&> \(PerPerson (Person fullName i) uids city loc) -> permutations (T.splitOn " " fullName)
            <&> \fullNameWords -> PerPerson (Person fullNameWords i) uids city loc

withRuz :: MonadIO io => [PerPerson Text] -> io [PerPerson Text]
withRuz perPerson = liftIO do
    pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length perPerson)
                             }
    concat <$> withTaskGroup 4 \g -> flip (mapConcurrently g) (permutFullNames perPerson)
        \(PerPerson (Person fullNameWords _) uids city loc) ->
            (query Ruz fullNameWords <* tick pb)
                <&> fmap \(QueryResult lbl descr) ->
                          PerPerson (Person lbl descr) uids city loc

hseGroups :: [(Text, GroupId)]
hseGroups =
    fmap GroupId <$>
        [ ("hsemem", 139105204)
        , ("prostpolitika", 160121249)
        , ("ombudsman_hse", 203966578)
        , ("hse_overheard", 57354358)
        , ("curatorhse", 128354471)
        , ("hse_cc", 185974612)
        , ("hseapp", 150048323)
        , ("hse_digital", 203687363)
        , ("hse_social", 165826930)
        , ("studsovetgsb", 135324713)
        , ("hsenn_career", 41221467)
        , ("ideal_hse", 185550746)
        , ("permhsecouncil", 122981522)
        , ("hse_go", 37731763)
        , ("hse_mezhcampus", 89964203)
        , ("studentdiscussion", 189288871)
        , ("hsecouncil_fandom", 207486284)
        , ("bakalavrik_hse_art_and_design", 196937408)
        , ("hsedesign", 21317467)
        , ("pravohseru", 4565)
        , ("doxajournal", 128503206)
        , ("patsanskoepravo", 130311951)
        , ("vsheviypolit", 171209032)
        , ("hsecouncil", 64952366) ]

accessToken :: Text
accessToken = "bcea24a641ab3935996574bfd873a09e722c0aca4131362dec5403ae3eec233389b5b5258cb399bc50e7b"

hseUniId :: UniversityId
hseUniId = UniversityId 128

findHseGroupsIn :: Set GroupId -> [(Text, GroupId)]
findHseGroupsIn grps = filter (\(_, gid) -> gid `Set.member` grps) hseGroups

data VKAccMatch = MatchUni UserId
                | GroupsMatch UserId [(Text, GroupId)]

matchId :: VKAccMatch -> UserId
matchId (MatchUni uid) = uid
matchId (GroupsMatch uid _) = uid

findVKAccount :: MonadIO io => Text -> io [VKAccMatch]
findVKAccount fullName = runVKTIO accessToken do
    liftIO (T.putStrLn fullName)
    usrsRes <- usersSearch (UsersSearchRequest (Just fullName) Nothing)
    usrs <- case usrsRes of
        Right ok -> pure ( ok)
        Left err -> Prelude.error (T.unpack err)
    let usrsWithUni =
            filter (\u -> hseUniId `elem` (universityId <$> fromMaybe [] (universities u))) usrs
    let usrsNoUni =
            filter (null . universities) usrs
    usrsWithGrps <- if null usrsNoUni then pure [] else do
        pb <- liftIO $ newProgressBar def { pgFormat = "Getting subs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                                          , pgTotal = toInteger (length usrsNoUni)
                                          }
        catMaybes <$> for usrsNoUni \(User uid _ _ _) -> do
            subsRes <- getSubscriptions (GetSubscriptionsRequest uid)
            liftIO (tick pb)
            case Set.fromList <$> subsRes of
                Right subs | length (findHseGroupsIn subs) > 0 -> pure (Just (uid, findHseGroupsIn subs))
                _ -> pure Nothing
    pure $ fmap (MatchUni . userId) usrsWithUni
            ++ fmap (\(uid, grps) -> GroupsMatch uid grps) usrsWithGrps

withVK :: [PerPerson Text] -> IO [PerPerson Text]
withVK people = do
    pb <- newProgressBar def { pgFormat = "Finding accs :percent [:bar] :current/:total elapsed :elapseds, ETA :etas"
                             , pgTotal = toInteger (length people)
                             }
    startEvalMemoT $
        for people \(PerPerson (Person fullName info) _ city loc) ->
            (memo findVKAccount fullName <* liftIO (tick pb)) <&>
                (\uids -> PerPerson (Person fullName info) uids city loc) . fmap matchId

main :: IO ()
main = displayConsoleRegions do
    rawHtml <- T.readFile "raw.html"
    parsed <- parseByCity rawHtml
    final <- withRuz (flattenByCity parsed) >>= withVK
    for_ final (T.putStrLn . perPersonToCsv)
