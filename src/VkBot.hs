module VkBot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import VkAPI (echo, getTsAndPts)

startVkBot :: Handle -> ConfigModules -> IO ()
startVkBot hLogger botConf = do
    logInfo hLogger "Bot started"
    logInfo hLogger "Checking token"
    chToken <- getTsAndPts hLogger (Config.token botConf)
    case chToken of
        Nothing -> logError hLogger "Bad token or server is not available"
        Just (ts, pts) -> do
            logInfo hLogger "Good token"
            echo hLogger (Config.token botConf) (Config.help botConf) [] ts pts
