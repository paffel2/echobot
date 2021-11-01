module Telegram.Bot where

import           Config                  (BotConfig (help, token))
import           Logger                  (LogHandle, logError, logInfo)
import           Telegram.Echo           (echo)
import           Telegram.TelegramHandle (TelegramHandle (getMe))
import           Telegram.Types          (HelpMessage (HelpMessage),
                                          TelegramToken (TelegramToken),
                                          UpdateId (UpdateId))

import           UsersLists              (RepeatsList)

startTelegramBot :: LogHandle IO -> TelegramHandle IO -> BotConfig -> IO ()
startTelegramBot hLogger hTelegram botConf = do
    logInfo hLogger "New Bot Start"
    logInfo hLogger "Check token"
    ch <- getMe hTelegram hLogger $ TelegramToken (Config.token botConf)
    case ch of
        Nothing -> logError hLogger "Bad token"
        Just _ -> do
            logInfo hLogger "Good token"
            loopBot
                hLogger
                hTelegram
                (TelegramToken (Config.token botConf))
                (HelpMessage (Config.help botConf))
                (Just $ UpdateId 0)
                []

loopBot ::
       Monad m
    => LogHandle m
    -> TelegramHandle m
    -> TelegramToken
    -> HelpMessage
    -> Maybe UpdateId
    -> RepeatsList
    -> m ()
loopBot hLogger hTelegram tgtoken help_message updId listOfRepeats = do
    (newUpdId, newListOfRepeat) <-
        echo hLogger hTelegram tgtoken updId help_message listOfRepeats
    loopBot hLogger hTelegram tgtoken help_message newUpdId newListOfRepeat
