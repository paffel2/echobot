module TelegramBot where
--import API
import Config
import Logger
import TelegramApi
--import Prelude (error)

{-startTelegramBot :: Handle -> ConfigModules -> IO ()
startTelegramBot hLogger botConf = do
    logInfo hLogger "Bot Start"
    echo''' hLogger (Config.token botConf) (Just 0) (Config.help botConf) [] -}
startTelegramBot :: Handle -> ConfigModules -> IO ()
startTelegramBot hLogger botConf = do
    logInfo hLogger "Bot Start"
    echo hLogger (Config.token botConf) (Just 0) (Config.help botConf) []
