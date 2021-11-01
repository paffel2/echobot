{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.Configurator as C
import           Logger            (Priority (..))

type VKBot = String

type HelpMessage = String

type Token = String

type TelegramBot = String

data BotType
    = VKBot
    | TelegramBot
    deriving (Show, Read)

data BotConfig =
    BotConfig
        { bot_type     :: BotType
        , token        :: Token
        , help         :: HelpMessage
        , log_priority :: Priority
        }

getConfig :: IO BotConfig
getConfig = do
    conf <- C.load [C.Optional "config/bot.conf"]
    bot_type_param <- C.lookupDefault "" conf "bot.type" :: IO String
    token_param <- C.lookupDefault "" conf "bot.token"
    help_param <- C.lookupDefault "" conf "bot.help"
    log_priority_param <-
        C.lookupDefault "Info" conf "logger.priority" :: IO String
    return $
        BotConfig
            { bot_type =
                  case bot_type_param of
                      "VKBot"       -> VKBot
                      "TelegramBot" -> TelegramBot
                      _             -> TelegramBot
            , token = token_param
            , help = help_param
            , log_priority =
                  case log_priority_param of
                      "Debug"   -> Debug
                      "Info"    -> Info
                      "Warning" -> Warning
                      "Error"   -> Logger.Error
                      _         -> Info
            }
