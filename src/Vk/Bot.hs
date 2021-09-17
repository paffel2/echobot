module Vk.Bot where

import Config (ConfigModules(help, token))
import Control.Concurrent (threadDelay)
import Logger (Handle, logError, logInfo)
import UsersLists (RepeatsList)
import Vk.Echo (echo)
import Vk.Types (HelpMessage, Pts, Ts, VkToken)
import Vk.VkHandle (VKHandle(getTsAndPts))

loopBot ::
       Handle IO
    -> VKHandle IO
    -> VkToken
    -> HelpMessage
    -> RepeatsList
    -> Ts
    -> Pts
    -> IO ()
loopBot hLogger hVk token' help_message [] ts pts = do
    newLoopInformation <- echo hLogger hVk token' help_message [] ts pts
    case newLoopInformation of
        Nothing -> do
            logError hLogger "Bad token or server is not available"
        Just ((ts', pts'), newList) -> do
            threadDelay 3000000
            loopBot hLogger hVk token' help_message newList ts' pts'
loopBot hLogger hVk token' help_message users_list ts pts = do
    newLoopInformation <- echo hLogger hVk token' help_message users_list ts pts
    case newLoopInformation of
        Nothing -> do
            logError hLogger ""
        Just ((ts', pts'), newList) -> do
            threadDelay 3000000
            loopBot hLogger hVk token' help_message newList ts' pts'

startVkBot :: Handle IO -> VKHandle IO -> ConfigModules -> IO ()
startVkBot hLogger hVK botConf = do
    logInfo hLogger "Bot started"
    logInfo hLogger "Checking token"
    chToken <- getTsAndPts hVK hLogger (Config.token botConf)
    case chToken of
        Nothing -> do
            logError hLogger "Bad token or server is not available"
        Just (ts, pts) -> do
            logInfo hLogger "Good token"
            loopBot
                hLogger
                hVK
                (Config.token botConf)
                (Config.help botConf)
                []
                ts
                pts
