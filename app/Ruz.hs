module Ruz where

import Data.Functor ((<&>))

import Data.Text (Text)
import Data.Text qualified as T

import Data.Set qualified as Set

import Network.HTTP.Req

import QueryPerson

data Ruz = Ruz
instance QueryPerson Ruz where
    query Ruz queryWords = do
        let url = https "ruz.hse.ru" /: "api" /: "search"
        runReq defaultHttpConfig
            (req GET url NoReqBody (jsonResponse @[QueryResult])
                ("term" =: T.unwords queryWords <> "type" =: ("student" :: Text)))
            <&> responseBody
            <&> filter (\(QueryResult lbl _) -> let lblWords = T.words lbl
                                                 in Set.fromList queryWords `Set.isSubsetOf` Set.fromList lblWords)