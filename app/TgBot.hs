module TgBot where

import Control.Concurrent (forkIO)
import Control.Monad.Representable.Reader (ask)
import Control.Monad.Writer.Lazy
import Data.Function ((&))
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Servant.Client (runClientM)
import Telegram.Bot.API.GettingUpdates
import Telegram.Bot.API.Types
import Telegram.Bot.Simple.BotApp
import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Instances ()
import Telegram.Bot.Simple.Reply
import VK.Find

data Action
  = Start
  | FindPerson
      { _author :: User
      , _fullNames :: [Text]
      }

bot :: [Text] -> BotApp [Text] Action
bot accessTokens =
  BotApp
    { botInitialModel = accessTokens
    , botAction = action
    , botHandler = handler
    , botJobs = []
    }

action :: Update -> [Text] -> Maybe Action
action Update {updateMessage = Just Message {messageText = Just "/start"}} _ = Just Start
action
  Update
    { updateMessage =
      Just
        Message
          { messageFrom = Just from
          , messageText = Just text
          }
    }
  _ = Just $ FindPerson from (T.lines text)
action _ _ = Nothing

handler :: Action -> [Text] -> Eff Action [Text]
handler Start accessTokens = pure accessTokens
handler (FindPerson _author fullNames) accessTokens =
  accessTokens <# do
    replyText ("Ищу " <> (T.pack $ show $ length fullNames) <> " имён")
    forkBotM do
      result <- findInVKMany accessTokens fullNames
      replyText $
        T.unlines $
          result <&> \case
            (name, Left err) -> name <> ": ошибка: " <> err
            (name, Right ok) -> personMatchToCsv (name, ok)

-- deriving newtype instance Alternative BotM

-- instance Alternative ClientM where

forkBotM :: BotM () -> BotM ()
forkBotM act = do
  botCtxt <- ask
  liftClientM do
    clientEnv <- ask
    act
      & runBotM botCtxt
      & flip runClientM clientEnv
      & (>>= either (fail . ("IT IS HERE: " <>) . show) pure)
      & forkIO
      & void
      & liftIO
