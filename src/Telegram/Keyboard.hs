module Telegram.Keyboard where

import Telegram.Responses
    ( TelegramInlineKeyboardButton(TelegramInlineKeyboardButton)
    , TelegramInlineKeyboardMarkup(TelegramInlineKeyboardMarkup)
    )

buttons :: [[TelegramInlineKeyboardButton]]
buttons = [[TelegramInlineKeyboardButton x x] | x <- ["1", "2", "3", "4", "5"]]

keyboard :: TelegramInlineKeyboardMarkup
keyboard = TelegramInlineKeyboardMarkup buttons
