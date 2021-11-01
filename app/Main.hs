module Main where

import           Config                  (BotConfig (..),
                                          BotType (TelegramBot, VKBot),
                                          getConfig)
import           Logger                  (LogHandle (LogHandle), printLog)
import           Telegram.Bot            (startTelegramBot)
import           Telegram.TelegramHandle (telegramHandler)
import           Vk.Bot                  (startVkBot)
import           Vk.VkHandle             (handlerVk)

main :: IO ()
main = do
    confBot <- getConfig
    --confLogger <- getLgConfig hConfig
    --let confLogger = LogHandle ()
    case bot_type confBot of
        VKBot ->
            startVkBot
                (LogHandle (log_priority confBot) printLog)
                handlerVk
                confBot
        TelegramBot ->
            startTelegramBot
                (LogHandle (log_priority confBot) printLog)
                telegramHandler
                confBot
