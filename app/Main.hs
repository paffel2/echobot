{-# LANGUAGE OverloadedStrings #-}
module Main where

--import Lib
import Logger ( printLog, Handle(Handle) )
import Config
    ( getBtConfig,
      getLgConfig,
      newConfigHandle,
      BotType(TelegramBot, VKBot),
      ConfigModules(bot_type, log_priority) )
import TelegramBot ( startTelegramBot )
import VkBot


main :: IO ()
main = do
    hConfig <- newConfigHandle
    confBot <- getBtConfig hConfig
    confLogger <- getLgConfig hConfig
    case bot_type confBot of
      VKBot -> startVkBot (Handle (log_priority confLogger) printLog) confBot
      TelegramBot -> startTelegramBot (Handle (log_priority confLogger) printLog) confBot
    

