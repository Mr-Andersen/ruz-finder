module Utils where

import Data.Text (Text)
import Data.Text qualified as T

import Polysemy -- (Sem, Members)
import Polysemy.Error (Error, throw)
import Polysemy.Log (Log, interpretLogStderr')
import Polysemy.Resource (Resource, resourceToIO)
import Polysemy.Conc (Mask)
import Polysemy.Conc.Interpreter.Mask (Restoration)

import Polysemy.Http
import Polysemy.Http.Data.Request (query)
import Network.HTTP.Client (BodyReader)

import Control.Lens

import System.Environment

import Data.Aeson (FromJSON, eitherDecode)

import Control.Concurrent.Async.Pool (TaskGroup)
import Control.Concurrent.Async.Pool qualified as A

getJSON :: ('[ Http BodyReader, Error Text ] `Members` r, FromJSON response)
        => Host -> Path -> [(QueryKey, Maybe QueryValue)]
        -> Sem r response
getJSON host path q = do
    result <- get host path
        & query .~ q
        & request @BodyReader
    case result of
        Left err -> throw $ T.pack $ show err
        Right (Success _ content _) -> case eitherDecode content of
            Left err -> throw $ T.pack err
            Right ok -> pure ok
        Right resp -> throw $ T.pack $ "Unhandled response code: " ++ show resp

withTaskGroup :: Embed IO `Member` r
              => (Sem r a -> Sem '[ Embed IO ] (m a))
              -> (m a -> Sem r a)
              -> Int -> (TaskGroup -> Sem r a)
              -> Sem r a
withTaskGroup absorb isolate n h = embed (A.withTaskGroup n (runM . absorb . h)) >>= isolate

mapConcurrently :: (Embed IO `Member` r, Traversable t)
                => (Sem r b -> Sem '[ Embed IO ] (m b))
                -> (m b -> Sem r b) -- t (m b) -> Sem r (t b)
                -> TaskGroup -> (a -> Sem r b) -> t a
                -> Sem r (t b)
mapConcurrently absorb isolate g h xs =
    embed (A.mapConcurrently g (runM . absorb . h) xs) >>= traverse isolate

interpretHttpIO :: Embed IO `Member` r => InterpretersFor '[ Http BodyReader ] r
interpretHttpIO = resourceToIO . interpretLogStderr' . interpretHttpNative . raiseUnder . raiseUnder

throwT :: Error Text `Member` r => Text -> Sem r a
throwT = throw
