module Telegram.Bot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import Telegram.Echo (echo)
import Telegram.TelegramHandle (TelegramHandle(getMe))
import Telegram.Types
    ( UpdateId(UpdateId),
      HelpMessage(HelpMessage),
      TelegramToken(TelegramToken) )
   
startTelegramBot :: Handle -> TelegramHandle -> ConfigModules -> IO ()
startTelegramBot hLogger hTelegram botConf = do
    logInfo hLogger "New Bot Start"
    logInfo hLogger "Check token"
    ch <- getMe hTelegram hLogger $ TelegramToken (Config.token botConf)
    case ch of
        Nothing -> logError hLogger "Bad token"
        Just _ -> do
            logInfo hLogger "Good token"
            echo
                hLogger
                hTelegram
                (TelegramToken (Config.token botConf))
                (Just $ UpdateId 0)
                (HelpMessage $ Config.help botConf)
                []
