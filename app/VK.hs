module VK where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

data VKT m a = VKT
    { runVKT :: m a
    , accessToken :: Text
    }

-- usersSearch :: 
