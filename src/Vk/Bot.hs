module Vk.Bot where

import Config (ConfigModules(help, token))
import Logger (Handle, logError, logInfo)
import Vk.Echo (echo)
import Vk.VkHandle (VKHandle(getTsAndPts))

startVkBot :: Handle -> VKHandle -> ConfigModules -> IO ()
startVkBot hLogger hVK botConf = do
    logInfo hLogger "Bot started"
    logInfo hLogger "Checking token"
    chToken <- getTsAndPts hVK hLogger (Config.token botConf)
    case chToken of
        Nothing -> logError hLogger "Bad token or server is not available"
        Just (ts, pts) -> do
            logInfo hLogger "Good token"
            echo
                hLogger
                hVK
                (Config.token botConf)
                (Config.help botConf)
                []
                ts
                pts
