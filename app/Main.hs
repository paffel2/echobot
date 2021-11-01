module Main where

import           Config                  (BotConfig (..),
                                          BotType (TelegramBot, VKBot),
                                          getConfig)

import           Control.Concurrent      (threadDelay)
import           Echo                    (startBot, telegramEchoHandler,
                                          vkEchoHandler)
import           Logger                  (LogHandle (LogHandle), printLog)
import           Telegram.TelegramHandle (telegramHandler)
import           Telegram.Types          (TelegramToken (TelegramToken))
import           Vk.Types                (VkToken (VkToken))
import           Vk.VkHandle             (handlerVk)

delayTime :: Int
delayTime = 3000000

main :: IO ()
main = do
    confBot <- getConfig
    case bot_type confBot of
        VKBot ->
            startBot
                (LogHandle (log_priority confBot) printLog)
                (vkEchoHandler
                     (VkToken (token confBot))
                     handlerVk
                     (threadDelay delayTime))
                confBot
        TelegramBot ->
            startBot
                (LogHandle (log_priority confBot) printLog)
                (telegramEchoHandler
                     (TelegramToken (token confBot))
                     telegramHandler
                     (return ()))
                confBot
