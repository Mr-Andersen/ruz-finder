module Main where

import Data.Foldable (for_)
-- import Data.Traversable (for)
-- import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (permutations)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Control.Concurrent.Async.Pool

import System.Console.AsciiProgress

import OVD.Types
import OVD.HTML
import QueryPerson
import Ruz

permutFullNames :: [PerPerson Text] -> [PerPerson [Text]]
permutFullNames perPerson =
    concat $ perPerson
        <&> \(PerPerson (Person fullName i) city loc) -> permutations (T.splitOn " " fullName)
            <&> \fullNameWords -> PerPerson (Person fullNameWords i) city loc

withRuz :: [PerPerson Text] -> IO [PerPerson Text]
withRuz perPerson = do
    pb <- newProgressBar def { pgFormat = "Quering RUZ :percent [:bar] :current/:total (:elapseds/~:etas)"
                             , pgTotal = toInteger (length perPerson)
                             }
    concat <$> withTaskGroup 4 \g -> flip (mapConcurrently g) (permutFullNames perPerson)
        \(PerPerson (Person fullNameWords _) city loc) ->
            (query Ruz fullNameWords <* tick pb)
                <&> fmap \(QueryResult lbl descr) ->
                          PerPerson (Person lbl descr) city loc

main :: IO ()
main = displayConsoleRegions do
    rawHtml <- T.readFile "raw.html"
    parsed <- parseByCity rawHtml
    withR <- withRuz (flattenByCity parsed)
    for_ withR (T.putStrLn . perPersonToCsv)
