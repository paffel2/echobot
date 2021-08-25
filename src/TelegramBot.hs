module TelegramBot where

import Config (ConfigModules)
import Logger (Handle)
import TelegramApi (startTelegramBot')

startTelegramBot :: Handle -> ConfigModules -> IO ()
startTelegramBot = startTelegramBot'
