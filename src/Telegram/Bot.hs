module Telegram.Bot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import Telegram.API (getMe)
import Telegram.Echo (echo)

startTelegramBot :: Handle -> ConfigModules -> IO ()
startTelegramBot hLogger botConf = do
    logInfo hLogger "Bot Start"
    logInfo hLogger "Check token"
    ch <- getMe hLogger (Config.token botConf)
    case ch of
        Nothing -> logError hLogger "Bad token"
        Just _ -> do
            logInfo hLogger "Good token"
            echo
                hLogger
                (Config.token botConf)
                (Just 0)
                (Config.help botConf)
                []
