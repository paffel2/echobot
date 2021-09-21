module Telegram.Types where

newtype TelegramToken = TelegramToken {tg_token :: String}
newtype HelpMessage = HelpMessage {help_mess :: String}
newtype UpdateId = UpdateId {upd_id :: Int}
newtype ChatId = ChatId {chat_id' :: Int} deriving Eq
newtype RepeatsNum = RepeatsNum {repeats_num' :: Int}
type RepeatsList = [Repeats]
newtype Caption = Caption {caption :: String}
newtype StatusResult = StatusResult {status_result :: Int}

data Repeats = Repeats {chat_id :: ChatId, repeats_num :: RepeatsNum} 