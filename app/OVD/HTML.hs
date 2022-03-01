module OVD.HTML where

import Control.Applicative
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import Data.Text qualified as T

import Text.HTML.Scalpel
import Text.RE.TDFA.Text as P

import OVD.Types

parserByCity :: SerialScraperT Text IO [ByCity Text]
parserByCity = many do
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

selectBody :: Selector
selectBody = "div" @: (hasClass <$> ["field", "field-name-body", "field-type-text-with-summary", "field-label-hidden"])

parseByCity :: Text -> IO [ByCity Text]
parseByCity rawHtml = fromMaybe [] <$> scrapeStringLikeT rawHtml (chroot selectBody (inSerial parserByCity))
