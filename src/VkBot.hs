module VkBot where

import Config (ConfigModules(help, token))
import Logger (Handle, logInfo)
import VkAPI (echo)

startVkBot :: Handle -> ConfigModules -> IO ()
startVkBot hLogger botConf = do
    logInfo hLogger "Bot started"
    echo hLogger (Config.token botConf) Nothing Nothing (Config.help botConf) []

