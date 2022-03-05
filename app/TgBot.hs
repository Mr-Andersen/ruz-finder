module TgBot where

import Data.Functor

import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.Writer.Lazy

import Telegram.Bot.Simple.BotApp
import Telegram.Bot.Simple.Eff
import Telegram.Bot.API.GettingUpdates
import Telegram.Bot.API.Types
import Telegram.Bot.Simple.Reply

import VK.Find

data Action = FindPerson { _author :: User
                         , _fullNames :: [Text]
                         }
            | SendResult { _author :: User
                         , _result :: Text
                         }

bot :: Text -> BotApp Text Action
bot accessToken = BotApp
    { botInitialModel = accessToken
    , botAction = action
    , botHandler = handler
    , botJobs = [] }

action :: Update -> Text -> Maybe Action
action Update { updateMessage = Just Message { messageFrom = Just from
                                             , messageText = Just text
                                             }
              } _ = Just $ FindPerson from (T.lines text)
action _ _ = Nothing

handler :: Action -> Text -> Eff Action Text
handler (FindPerson author fullNames) accessToken =
    Eff do
        tell
            [ replyText ("Ищу " <> (T.pack $ show $ length fullNames) <> " имён") $> Nothing
            , Just . SendResult author <$> 
                (findInVKMany accessToken fullNames <&> fmap personMatchToCsv <&> T.unlines)
            ]
        pure accessToken
handler (SendResult _author result) accessToken =
    Eff do
        tell [ replyText result $> Nothing ]
        pure accessToken
