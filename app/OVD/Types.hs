module OVD.Types where

import Data.Functor ((<&>))

import Data.Text (Text)
import Data.Text qualified as T

data Person f = Person { _fullName :: f, _info :: Text }
instance Show (Person Text) where
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

data ByLoc f = ByLoc { _loc :: Location, _people :: [Person f] }
    -- deriving (Show)
data ByCity f = ByCity { _city :: City, _byLoc :: [ByLoc f] }
    -- deriving (Show)

data PerPerson f = PerPerson (Person f) City Location

flattenByCity :: [ByCity f] -> [PerPerson f]
flattenByCity byCity = do
    ByCity city byLoc <- byCity
    ByLoc loc people <- byLoc
    person <- people
    pure (PerPerson person city loc)

perPersonToCsv :: PerPerson Text -> Text
perPersonToCsv (PerPerson (Person fullName info) (City city) loc) =
    T.intercalate "," [fullName, info, city, locToCsv loc]

byCityToCsv :: [ByCity Text] -> [Text]
byCityToCsv byCity = flattenByCity byCity <&> perPersonToCsv
