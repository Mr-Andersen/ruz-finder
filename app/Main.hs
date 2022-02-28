module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Foldable (for_)
-- import Data.Traversable (for)
-- import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.List (permutations)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Text.HTML.Scalpel

import Text.RE.TDFA.Text as P

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

import Network.HTTP.Req

import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress

data Person = Person { _fullName :: Text, _info :: Text }
instance Show Person where
    show (Person p i) = "Person: " ++ T.unpack p ++ "(" ++ T.unpack i ++ ")"

data Location = Location Text | LocationUnknown
instance Show Location where
    show (Location x) = "Location: " ++ T.unpack x
    show LocationUnknown = "Location: ?"
locToCsv :: Location -> Text
locToCsv (Location x) = x
locToCsv LocationUnknown = ""

newtype City = City { unCity :: Text }
instance Show City where
    show (City x) = "City: " ++ T.unpack x

data ByLoc = ByLoc { _loc :: Location, _people :: [Person] }
    deriving (Show)
data ByCity = ByCity { _city :: City, _byLoc :: [ByLoc] }
    deriving (Show)

data PerPerson = PerPerson Person City Location

flattenByCity :: [ByCity] -> [PerPerson]
flattenByCity byCity = do
    ByCity city byLoc <- byCity
    ByLoc loc people <- byLoc
    person <- people
    pure (PerPerson person city loc)

perPersonToCsv :: PerPerson -> Text
perPersonToCsv (PerPerson (Person fullName info) (City city) loc) =
    T.intercalate "," [fullName, info, city, locToCsv loc]

byCityToCsv :: [ByCity] -> [Text]
byCityToCsv byCity = flattenByCity byCity <&> perPersonToCsv

parseByCity :: SerialScraperT Text IO [ByCity]
parseByCity = many do
    city <- City <$> seekNext (text "h2")
    byLoc <- many do
        locRaw <- seekNext (text "h3")
        let loc = if "неизвестно" `T.isInfixOf` locRaw then LocationUnknown else Location locRaw
        peopleRaw <- seekNext (text "p")
        let pattern = [re|[А-ЯЁ][а-яё]+(-| )[А-ЯЁ][а-яё]+|]
        let fullNames = P.matches (peopleRaw *=~ pattern)
        let people = flip Person "" <$> fullNames
        pure (ByLoc loc people)
    pure (ByCity city byLoc)

data QueryResult = QueryResult
    { label :: Text
    , description :: Text }
    deriving (Generic)

instance FromJSON QueryResult

class QueryPerson s where
    query :: MonadIO io => s -> Text -> io [QueryResult]

data Ruz = Ruz
instance QueryPerson Ruz where
    query Ruz queryLine = do
        let url = https "ruz.hse.ru" /: "api" /: "search"
        runReq defaultHttpConfig
            (req GET url NoReqBody (jsonResponse @[QueryResult])
                ("term" =: queryLine <> "type" =: ("student" :: Text)))
            <&> responseBody

permutFullNames :: [PerPerson] -> [PerPerson]
permutFullNames perPerson =
    concat $ perPerson
        <&> \(PerPerson (Person fullName i) city loc) -> permutations (T.splitOn " " fullName)
            <&> \wordSeq -> let fullName' = T.intercalate " " wordSeq in
                PerPerson (Person fullName' i) city loc

withRuz :: [PerPerson] -> IO [PerPerson]
withRuz perPerson = do
    pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total (:elapseds/~:etas)"
                             , pgTotal = toInteger (length perPerson)
                             }
    concat <$> withTaskGroup 4 \g -> flip (mapConcurrently g) (permutFullNames perPerson)
        \(PerPerson (Person fullName _) city loc) ->
            (query Ruz fullName <* tick pb)
                <&> fmap \(QueryResult lbl descr) ->
                          PerPerson (Person lbl descr) city loc

selectBody :: Selector
selectBody = "div" @: (hasClass <$> ["field", "field-name-body", "field-type-text-with-summary", "field-label-hidden"])

main :: IO ()
main = displayConsoleRegions do
    rawHtml <- T.readFile "raw.html"
    parsed <- fromMaybe [] <$> scrapeStringLikeT rawHtml (chroot selectBody (inSerial parseByCity))
    withR <- withRuz (flattenByCity parsed)
    for_ withR (T.putStrLn . perPersonToCsv)
