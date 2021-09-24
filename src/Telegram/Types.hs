module Telegram.Types where

import GHC.Generics ( Generic )
import Data.Aeson.Types ( Parser )
import Data.Aeson ( ToJSON(toJSON), FromJSON(parseJSON) )

newtype TelegramToken = TelegramToken {tg_token :: String}
newtype HelpMessage = HelpMessage {help_mess :: String}
newtype UpdateId = UpdateId {upd_id :: Int} deriving (Eq,Show,Generic)
newtype ChatId = ChatId {chat_id' :: Int} deriving (Eq,Generic,Show)
newtype RepeatsNum = RepeatsNum {repeats_num' :: Int} deriving (Eq,Show,Generic)
type RepeatsList = [Repeats]
newtype Caption = Caption {caption :: String} deriving (Eq,Show,Generic)
newtype StatusResult = StatusResult {status_result :: Int}

data Repeats = Repeats {chat_id :: ChatId, repeats_num :: RepeatsNum} 


instance FromJSON ChatId where
    parseJSON v =  ChatId <$> (parseJSON v :: Parser Int)
instance ToJSON ChatId where
    toJSON = toJSON . chat_id' 

instance FromJSON UpdateId where
    parseJSON v = UpdateId <$> (parseJSON v :: Parser Int)

instance FromJSON Caption where
    parseJSON v = Caption <$> (parseJSON v :: Parser String)
instance ToJSON Caption where
    toJSON = toJSON . caption

instance FromJSON RepeatsNum where
    parseJSON v = RepeatsNum . read <$> (parseJSON v :: Parser String)