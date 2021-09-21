module Telegram.Types where

newtype TelegramToken = TelegramToken { tg_token :: String}
newtype HelpMessage = HelpMessage {help_mess :: String}
newtype UpdateId = UpdateId {upd_id :: Int} deriving (Eq,Show)
--newtype ChatId = ChatId {chat_id' :: Int}
newtype Caption = Caption {caption :: String}
newtype StatusResult = StatusResult {status_result ::Int} deriving (Eq,Show)

