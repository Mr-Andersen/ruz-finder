{-# LANGUAGE Strict #-}

module Main where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.HTML.Scalpel

import Text.Regex.Base
import Text.Regex.PCRE ((=~), configUTF8)

newtype Person = Person { unPerson :: Text }
instance Show Person where
    show (Person x) = "Person: " ++ T.unpack x

newtype Location = Location { unLocation :: Text }
instance Show Location where
    show (Location x) = "Location: " ++ T.unpack x

newtype City = City { unCity :: Text }
instance Show City where
    show (City x) = "City: " ++ T.unpack x

data ByCity = ByCity { _city :: City, _byLoc :: [ByLoc] }
    deriving (Show)
data ByLoc = ByLoc { _loc :: Location, _people :: [Person] }
    deriving (Show)

-- takeWhileCan :: Monad m => m (Either e a) -> m ([a], e)
-- takeWhileCan

parseByCity :: SerialScraperT Text IO [ByCity]
parseByCity = many do
    city <- City <$> seekNext (text "h2")
    byLoc <- many do
        loc <- Location <$> seekNext (text "h3")
        peopleRaw <- seekNext (text "p")
        let pattern = "[А-ЯЁ][а-яё]+[\\- ][А-ЯЁ][а-яё]+" :: String
        liftIO $ T.putStrLn peopleRaw
        let people = Person . T.pack <$> getAllTextMatches (T.unpack peopleRaw =~ pattern)
        liftIO $ print (length people)
        pure (ByLoc loc people)
    pure (ByCity city byLoc)

selectBody :: Selector
selectBody = "div" @: (hasClass <$> ["field", "field-name-body", "field-type-text-with-summary", "field-label-hidden"])

main :: IO ()
main = assert configUTF8 do
    rawHtml <- T.readFile "raw.html"
    scrapeStringLikeT rawHtml (chroot selectBody (inSerial parseByCity)) >>= print
