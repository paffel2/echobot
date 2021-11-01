module Telegram.Types where

import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types (Parser)
import           GHC.Generics     (Generic)

newtype TelegramToken =
    TelegramToken
        { tg_token :: String
        }

{-newtype HelpMessage =
    HelpMessage
        { help_mess :: String
        } -}
newtype UpdateId =
    UpdateId
        { upd_id :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON UpdateId where
    parseJSON v = UpdateId <$> (parseJSON v :: Parser Int)

newtype Caption =
    Caption
        { caption :: String
        }
    deriving (Eq, Show, Generic)

instance FromJSON Caption where
    parseJSON v = Caption <$> (parseJSON v :: Parser String)

instance ToJSON Caption where
    toJSON = toJSON . caption

newtype StatusResult =
    StatusResult
        { status_result :: Int
        }
    deriving (Eq, Show)
