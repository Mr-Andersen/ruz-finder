module Utils where

import Data.Functor

import Control.Monad.IO.Class
import Control.Monad.Except (ExceptT, runExceptT)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

runExceptTReporting :: MonadIO io => a -> ExceptT Text io a -> io a
runExceptTReporting defVal act = runExceptT act >>=
    \case
        Left err -> liftIO (T.putStrLn ("\nERROR: " <> err <> "\n")) $> defVal
        Right ok -> pure ok

-- Takes ФИО and returns ФИ
dropPatronymic :: Text -> Text
dropPatronymic = T.unwords . take 2 . T.words
