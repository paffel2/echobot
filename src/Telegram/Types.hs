module Telegram.Types where

newtype TelegramToken = TelegramToken {tg_token :: String}
type HelpMessage = String
type UpdateId = Int
type ChatId = Int
type RepeatsNum = Int
type RepeatsList = [Repeats]
type Caption = String
type StatusResult = Int

data Repeats = Repeats {chat_id :: ChatId, repeats_num :: RepeatsNum} 