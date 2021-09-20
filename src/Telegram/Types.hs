module Telegram.Types where

newtype TelegramToken = TelegramToken { tg_token :: String}
type HelpMessage = String
type UpdateId = Int
type ChatId = Int
type Caption = String
type StatusResult = Int

