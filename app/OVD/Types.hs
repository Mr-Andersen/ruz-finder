module OVD.Types where

import Control.Category ((>>>))

-- import Data.Function ((&))
-- import Data.Functor ((<&>))

import Data.Char (isAlpha, isSpace)

import Data.Text (Text)
import Data.Text qualified as T

-- import Data.Text.IO qualified as T

-- import VK qualified
import VK.Find qualified as VK

data Person f = Person {_fullName :: f, _info :: Text}
instance Show (Person Text) where
  show (Person p i) = "Person: " ++ T.unpack p ++ "(" ++ T.unpack i ++ ")"

data Location = Location Text | LocationUnknown
instance Show Location where
  show (Location x) = "Location: " ++ T.unpack x
  show LocationUnknown = "Location: ?"
locToCsv :: Location -> Text
locToCsv (Location x) = x
locToCsv LocationUnknown = ""

newtype City = City {unCity :: Text}
instance Show City where
  show (City x) = "City: " ++ T.unpack x

data ByLoc f = ByLoc {_loc :: Location, _people :: [Person f]}

-- deriving (Show)
data ByCity f = ByCity {_city :: City, _byLoc :: [ByLoc f]}

-- deriving (Show)

instance Show (ByCity Text) where
  show = unlines . fmap show . flattenByCity . (: [])

data PerPerson f = PerPerson (Person f) [VK.VKAccMatch] City Location

instance Show (PerPerson Text) where
  show = T.unpack . perPersonToCsv

perPersonFullName :: PerPerson f -> f
perPersonFullName (PerPerson (Person f _) _ _ _) = f

flattenByCity :: [ByCity f] -> [PerPerson f]
flattenByCity byCity = do
  ByCity city byLoc <- byCity
  ByLoc loc people <- byLoc
  person <- people
  pure (PerPerson person [] city loc)

perPersonToCsv :: PerPerson Text -> Text
perPersonToCsv (PerPerson (Person fullName info) mtchs (City city) loc) =
  T.intercalate
    ","
    [ fullName
    , T.unwords $ VK.vkAccMatchToCsv <$> mtchs
    , -- ((\(VK.UserId uid) -> "https://vk.com/id" <> T.pack (show uid)) <$> uids)
      info
    , city
    , locToCsv loc
    ]

byCityToCsv :: [ByCity Text] -> [Text]
byCityToCsv byCity = perPersonToCsv <$> flattenByCity byCity

trimFullName :: Text -> Text
trimFullName =
  T.unpack
    >>> filter (\c -> isAlpha c || isSpace c)
    >>> T.pack
    >>> T.strip

trimFullNames :: [PerPerson Text] -> [PerPerson Text]
trimFullNames = fmap \(PerPerson (Person fullName info) uids (City city) loc) ->
  (PerPerson (Person (trimFullName fullName) info) uids (City city) loc)
