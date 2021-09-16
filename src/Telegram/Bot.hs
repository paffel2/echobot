module Telegram.Bot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import Telegram.Echo (echo)
import Telegram.TelegramHandle (TelegramHandle(getMe))

startTelegramBot :: Handle IO -> TelegramHandle IO -> ConfigModules -> IO ()
startTelegramBot hLogger hTelegram botConf = do
    logInfo hLogger "New Bot Start"
    logInfo hLogger "Check token"
    ch <- getMe hTelegram hLogger (Config.token botConf)
    case ch of
        Nothing -> logError hLogger "Bad token"
        Just _ -> do
            logInfo hLogger "Good token"
            echo
                hLogger
                hTelegram
                (Config.token botConf)
                (Just 0)
                (Config.help botConf)
                []
