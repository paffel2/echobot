module Main where

import           Config       (BotConfig (..), BotType (TelegramBot, VKBot),
                               getConfig)

import           Logger       (LogHandle (LogHandle), printLog)
import qualified Telegram.Bot as Telegram
import qualified Vk.Bot       as VK

main :: IO ()
main = do
    confBot <- getConfig
    case bot_type confBot of
        VKBot -> VK.startBot (LogHandle (log_priority confBot) printLog) confBot
        TelegramBot ->
            Telegram.startBot
                (LogHandle (log_priority confBot) printLog)
                confBot
