{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Configurator as C
import qualified Data.Text as T
import Logger (Priority(..))

data Modules
    = BT
    | LG

type VKBot = String
type HelpMessage = String
type Token = String
type TelegramBot = String

data BotType
    = VKBot
    | TelegramBot
    deriving (Show)

data ConfigModules
    = Bot
          { bot_type :: BotType
          , token :: Token
          , help :: HelpMessage
          }
    | Log
          { log_priority :: Priority
          }

newtype ConfigHandle =
    ConfigHandle
        { getConfig :: Modules -> IO ConfigModules
        }

getBtConfig, getLgConfig :: ConfigHandle -> IO ConfigModules
getBtConfig = (`getConfig` BT)

getLgConfig = (`getConfig` LG)

newConfigHandle :: IO ConfigHandle
newConfigHandle = return $ ConfigHandle {getConfig = getconfig}

getconfig :: Modules -> IO ConfigModules
getconfig module' = do
    conf <- C.load [C.Optional "config/bot.conf"]
    case module' of
        BT -> do
            bot_type_param <-
                C.lookupDefault "" conf (T.pack "bot.type") :: IO String
            token_param <-
                C.lookupDefault "" conf (T.pack "bot.token") :: IO String
            help_param <-
                C.lookupDefault "" conf (T.pack "bot.help") :: IO String
            case bot_type_param of
                "VKBot" -> return $ Bot VKBot token_param help_param
                "TelegramBot" -> return $ Bot TelegramBot token_param help_param
                _ -> return $ Bot TelegramBot token_param help_param
        LG -> do
            log' <-
                C.lookupDefault "Info" conf (T.pack "logger.priority") :: IO String
            case log' of
                "Debug" -> return $ Log Debug
                "Info" -> return $ Log Info
                "Warning" -> return $ Log Warning
                "Error" -> return $ Log Logger.Error
                _ -> return $ Log Info
