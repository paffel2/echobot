module TelegramBot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import TelegramApi (echo, getMe)

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
--1431530804:AAH5bSr9xr8o3WQlF55hnmpYZYtktn-rzWY
