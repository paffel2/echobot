module VkBot where
import Logger
import Config
import VkAPI

startVkBot :: Handle -> ConfigModules -> IO ()
startVkBot hLogger botConf = do
    logInfo hLogger "Bot started"
    echo hLogger (Config.token botConf) Nothing Nothing (Config.help botConf) []


--"57787f7aaf82fde6b43ccae4c287835e3123ec05f979f325bb5471e002a11eecb8a4c359368cb053c572b"


--vkbottst :: IO ()
--vkbottst = echo (Handle Debug printLog) "57787f7aaf82fde6b43ccae4c287835e3123ec05f979f325bb5471e002a11eecb8a4c359368cb053c572b" Nothing Nothing "some help" []