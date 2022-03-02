{-# LANGUAGE Strict #-}

module OVD.HTML where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import Data.Text qualified as T

import Text.HTML.Scalpel
-- import Text.RE.PCRE.Text ((*=~), reMS)
-- import Text.RE.PCRE.Text qualified as P
import Text.RE.TDFA.Text ((*=~), reMS)
import Text.RE.TDFA.Text qualified as P

import OVD.Types

import Debug.Trace

parserByCity :: Monad m => SerialScraperT Text m [ByCity Text]
parserByCity = many do
    city <- City <$> seekNext (text "h2")
    -- _ <- fmap traceShowId $ seekNext (text "h2") <|> pure "NOT FOUND"
    byLoc <- untilNext (text "h2") $ many do
        locRaw <- seekNext (text "h3")
        let loc = if "неизвестно" `T.isInfixOf` locRaw then LocationUnknown else Location locRaw
        peopleRaw <- seekNext (text "p")
        let pattern = [reMS|[А-ЯЁ][а-яё\-]+ \n?[А-ЯЁ][а-яё\-]+|]
        let fullNames = P.matches (peopleRaw *=~ pattern)
        let people = fullNames <&> T.unpack <&> filter ('\n' /=) <&> T.pack <&> flip Person ""
        pure (ByLoc loc people)
    pure (ByCity city byLoc)

selectBody :: Selector
selectBody = "div" @: (hasClass <$> ["field", "field-name-body", "field-type-text-with-summary", "field-label-hidden"])

byCityInBody :: Monad m => ScraperT Text m [ByCity Text]
byCityInBody = chroot selectBody (inSerial parserByCity)

parseByCity :: Text -> IO [ByCity Text]
parseByCity rawHtml = fromMaybe [] <$> scrapeStringLikeT rawHtml (chroot selectBody (inSerial parserByCity))
