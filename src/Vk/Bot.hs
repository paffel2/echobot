module Vk.Bot where

import           Config             (BotConfig (help, token))
import           Control.Concurrent (threadDelay)
import           Logger             (LogHandle, logError, logInfo)
import           UsersLists         (RepeatsList)
import           Vk.Echo            (echo)
import           Vk.Types           (HelpMessage (HelpMessage), Pts, Ts,
                                     VkToken (VkToken))
import           Vk.VkHandle        (VKHandle (getTsAndPts))

delayTime :: Int
delayTime = 3000000

loopBot ::
       LogHandle IO
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
        Just ((newTs, newPts), newList) -> do
            threadDelay delayTime
            loopBot hLogger hVk token' help_message newList newTs newPts
loopBot hLogger hVk token' help_message users_list ts pts = do
    newLoopInformation <- echo hLogger hVk token' help_message users_list ts pts
    case newLoopInformation of
        Nothing -> do
            logError hLogger "Bad token or server is not available"
        Just ((newTs, newPts), newList) -> do
            threadDelay delayTime
            loopBot hLogger hVk token' help_message newList newTs newPts

startVkBot :: LogHandle IO -> VKHandle IO -> BotConfig -> IO ()
startVkBot hLogger hVK botConf = do
    logInfo hLogger "Bot started"
    logInfo hLogger "Checking token"
    chToken <- getTsAndPts hVK hLogger $ VkToken (Config.token botConf)
    case chToken of
        Nothing -> do
            logError hLogger "Bad token or server is not available"
        Just (ts, pts) -> do
            logInfo hLogger "Good token"
            loopBot
                hLogger
                hVK
                (VkToken (Config.token botConf))
                (HelpMessage (Config.help botConf))
                []
                ts
                pts
