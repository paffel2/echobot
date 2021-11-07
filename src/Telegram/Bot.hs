{-# LANGUAGE FlexibleContexts #-}

module Telegram.Bot where

import qualified Config                  as C

import           Control.Monad.Reader    (MonadIO (liftIO),
                                          ReaderT (runReaderT))
import           Control.Monad.State     (MonadState (get, put), StateT,
                                          evalStateT)
import           Echo                    (DataLoop (DataLoop), UserMessage,
                                          echo)
import           Logger                  (LogHandle, logError, logInfo)
import           Telegram.API            (getLastUpdateId, getMe, getUpdates)
import           Telegram.Impl           (fromTgUpdateToUserMessage)
import           Telegram.Responses      (TelegramUpdate, TgMessage)
import           Telegram.TelegramHandle (tgHandler)
import           Telegram.Types          (TelegramToken (TelegramToken),
                                          UpdateId)
import qualified UsersLists              as UL

tgGetMessage ::
       LogHandle IO
    -> TelegramToken
    -> Maybe UpdateId
    -> IO [Maybe (UserMessage TgMessage)]
tgGetMessage hLogger token lastUpdId = do
    upd <- getUpdates token hLogger lastUpdId
    case upd of
        Nothing      -> return []
        Just updates -> return $ fromTgUpdateToUserMessage <$> updates

updateUpdateId ::
       Maybe [TelegramUpdate]
    -> LogHandle IO
    -> StateT (DataLoop UpdateId) IO ()
updateUpdateId updates hLogger = do
    (DataLoop list _) <- get
    nextUpdateId <- liftIO $ getLastUpdateId updates hLogger
    put $ DataLoop list nextUpdateId

loopBot ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> StateT (DataLoop UpdateId) IO ()
loopBot hLogger token helpMessage = do
    (DataLoop _ updateId) <- get
    updates <- liftIO $ getUpdates token hLogger updateId
    let usersMessages =
            case updates of
                Nothing   -> []
                Just m_um -> fromTgUpdateToUserMessage <$> m_um
    let handler = tgHandler hLogger token helpMessage
    mapM_ (runReaderT (echo handler)) usersMessages
    updateUpdateId updates hLogger
    loopBot hLogger token helpMessage

startBot :: LogHandle IO -> C.BotConfig -> IO ()
startBot hLogger botConf = do
    liftIO $ logInfo hLogger "New Bot Start"
    liftIO $ logInfo hLogger "Check token"
    ch <- liftIO $ getMe (TelegramToken (C.token botConf)) hLogger
    case ch of
        Nothing -> liftIO $ logError hLogger "Bad token"
        _ -> do
            liftIO $ logInfo hLogger "Good token"
            evalStateT
                (loopBot
                     hLogger
                     (TelegramToken (C.token botConf))
                     (UL.HelpMessage $ C.help botConf))
                (DataLoop [] Nothing)
