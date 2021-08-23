module TelegramBot where
import Config
import Logger
import TelegramApi



startTelegramBot :: Handle -> ConfigModules -> IO ()
startTelegramBot = startTelegramBot'