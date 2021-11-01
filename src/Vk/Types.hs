module Vk.Types where

import           Data.Aeson       (FromJSON (parseJSON))
import           Data.Aeson.Types (Parser)
import           GHC.Generics     (Generic)

newtype VkToken =
    VkToken
        { vk_token :: String
        }

{-newtype HelpMessage =
    HelpMessage
        { help_mess :: String
        } -}
--https://vk.com/dev/messages.getLongPollHistory - about TS and PTS parameters
newtype Ts =
    Ts
        { getTs :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON Ts where
    parseJSON v = Ts <$> (parseJSON v :: Parser Int)

newtype Pts =
    Pts
        { getPts :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON Pts where
    parseJSON v = Pts <$> (parseJSON v :: Parser Int)
