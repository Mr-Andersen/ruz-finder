module Ruz where

import Data.Text qualified as T

import Polysemy.Http

import QueryPerson
import Utils

data Ruz = Ruz
instance QueryPerson Ruz where
    queryPerson Ruz queryWords =
        getJSON
            "ruz.hse.ru"
            "api/search"
            [ ("type", Just "student")
            , ("term", Just . QueryValue $ T.unwords queryWords) ]
