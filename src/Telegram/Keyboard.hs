module Telegram.Keyboard where

import           Telegram.Responses (TelegramInlineKeyboardButton (TelegramInlineKeyboardButton),
                                     TelegramInlineKeyboardMarkup (TelegramInlineKeyboardMarkup))

keyboard :: TelegramInlineKeyboardMarkup
keyboard =
    TelegramInlineKeyboardMarkup
        [ [TelegramInlineKeyboardButton "1" "1"]
        , [TelegramInlineKeyboardButton "2" "2"]
        , [TelegramInlineKeyboardButton "3" "3"]
        , [TelegramInlineKeyboardButton "4" "4"]
        , [TelegramInlineKeyboardButton "5" "5"]
        ]
