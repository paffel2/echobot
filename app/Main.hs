{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
    ( BotType(TelegramBot, VKBot)
    , ConfigModules(bot_type, log_priority)
    , getBtConfig
    , getLgConfig
    , newConfigHandle
    )
import Logger (Handle(Handle), printLog)
import TelegramBot (startTelegramBot)
import VkBot (startVkBot)

main :: IO ()
main = do
    hConfig <- newConfigHandle
    confBot <- getBtConfig hConfig
    confLogger <- getLgConfig hConfig
    case bot_type confBot of
        VKBot -> startVkBot (Handle (log_priority confLogger) printLog) confBot
        TelegramBot ->
            startTelegramBot (Handle (log_priority confLogger) printLog) confBot


