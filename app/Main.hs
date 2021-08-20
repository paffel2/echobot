{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Logger
import Config
import TelegramBot


main :: IO ()
main = do
    hConfig <- newConfigHandle
    confBot <- getBtConfig hConfig
    confLogger <- getLgConfig hConfig
    case bot_type confBot of
      VKBot -> print "In develop"
      TelegramBot -> startTelegramBot (Handle (log_priority confLogger) printLog) confBot
    

    {-main = do
    hConfig <- newConfigHandle
    confToken <- getTkConfig hConfig
    confLogger <- getLgConfig hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    let db_address = dbAddress confDb
    let token_lifetime = lifeTime confToken
    let hLogger = Handle (log_priority confLogger) printLog
    logInfo hLogger "Serving"
    runSettings
        (setMaximumBodyFlush (server_maximum_body_flush confServer) $
         setPort (server_port confServer) defaultSettings) $
        routes hLogger db_address token_lifetime handler-}
