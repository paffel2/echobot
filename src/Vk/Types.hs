module Vk.Types where

import Data.Aeson (FromJSON(parseJSON))
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic)

newtype VkToken =
    VkToken
        { vk_token :: String
        }

newtype HelpMessage =
    HelpMessage
        { help_mess :: String
        }

newtype Ts =
    Ts
        { ts' :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON Ts where
    parseJSON v = Ts <$> (parseJSON v :: Parser Int)

newtype Pts =
    Pts
        { pts' :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON Pts where
    parseJSON v = Pts <$> (parseJSON v :: Parser Int)
