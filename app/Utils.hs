module Utils where

import Control.Concurrent.Async.Pool (TaskGroup, mapConcurrently)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (stderr)

runExceptTReporting :: MonadIO io => ExceptT Text io a -> io (Either Text a)
runExceptTReporting act =
  runExceptT act
    >>= \case
      Left err -> do
        ePutTextLn $ "\nERROR: " <> err <> "\n"
        pure $ Left err
      Right ok -> pure $ Right ok

-- Takes ФИО and returns ФИ
dropPatronymic :: Text -> Text
dropPatronymic = T.unwords . take 2 . T.words

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs

retry :: MonadError e m => Int -> m a -> m a
retry 0 a = a
retry n a | n > 0 = catchError a $ const $ retry (n - 1) a
retry _ _ = error "retry: got negative number of retries"

ePutTextLn :: MonadIO io => Text -> io ()
ePutTextLn = liftIO . T.hPutStrLn stderr

showText :: Show a => a -> Text
showText = T.pack . show

forConcurrently :: Traversable t => TaskGroup -> t a -> (a -> IO b) -> IO (t b)
forConcurrently = flip . mapConcurrently
