{-# LANGUAGE FlexibleContexts #-}

module Telegram.TelegramBot where

import qualified Config               as C
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text            as T
import           Logger
import           Logic
import           Telegram.API
import           Telegram.Impl
import           Telegram.Responses
import           Telegram.Types
import qualified UsersLists           as UL

tgGetMessage ::
       LogHandle IO
    -> TelegramToken
    -> Maybe UpdateId
    -> IO [Maybe (UserMessage TgMessage')]
tgGetMessage hLogger token lastUpdId = do
    upd <- getUpdates token hLogger lastUpdId
    case upd of
        Nothing      -> return []
        Just updates -> return $ fromTgUpdateToUserMessage <$> updates

tgRepeatsByUser :: UL.ChatId -> StateT UL.RepeatsList IO (Maybe UL.RepeatsNum)
tgRepeatsByUser chatId = do
    list <- get
    return $ Just $ UL.findRepeatNumber list chatId

tgUpdateRepeatsForUser ::
       UL.ChatId -> UL.RepeatsNum -> StateT UL.RepeatsList IO ()
tgUpdateRepeatsForUser chatId repeatsNums = do
    list <- get
    put (UL.updateListUsers list [UL.Repeats chatId repeatsNums])

tgSendAnswer ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> BotMessage TgMessage'
    -> StateT UL.RepeatsList IO ()
tgSendAnswer hLogger tgToken helpMessage botMessage =
    case botMessageContent botMessage of
        PlainText s -> do
            _ <-
                liftIO $
                sendTextMessage hLogger tgToken (to botMessage) s Nothing
            return ()
        HelpMessage -> do
            _ <-
                liftIO $
                sendTextMessage
                    hLogger
                    tgToken
                    (to botMessage)
                    (UL.getHelpMessage helpMessage)
                    Nothing
            return ()
        Keyboard -> do
            _ <- liftIO $ sendKeyboard hLogger tgToken (to botMessage)
            return ()
        RepeatMessage rn tm -> repeatAnswer rn tm
  where
    repeatAnswer rn tm =
        when (UL.getRepeatsNum rn > 0) $ do
            oneAnswer tm
            repeatAnswer (UL.RepeatsNum (UL.getRepeatsNum rn - 1)) tm
    oneAnswer tm =
        case tm of
            TextMessage' text entity -> do
                _ <-
                    liftIO $
                    sendTextMessage hLogger tgToken (to botMessage) text entity
                liftIO $ logInfo hLogger "TextMessage sended."
            AnimationMessage' anim cap -> do
                _ <-
                    liftIO $
                    sendAnimationMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        anim
                        cap
                liftIO $ logInfo hLogger "Animation sended."
            AudioMessage' audio сap -> do
                _ <-
                    liftIO $
                    sendAudioMessage hLogger tgToken (to botMessage) audio сap
                liftIO $ logInfo hLogger "Audio sended."
            DocumentMessage' doc cap -> do
                _ <-
                    liftIO $
                    sendDocumentMessage hLogger tgToken (to botMessage) doc cap
                liftIO $ logInfo hLogger "Document sended."
            PhotoMessage' photo cap -> do
                _ <-
                    liftIO $
                    sendPhotoMessage hLogger tgToken (to botMessage) photo cap
                liftIO $ logInfo hLogger "Photo sended."
            VideoMessage' video cap -> do
                _ <-
                    liftIO $
                    sendVideoMessage hLogger tgToken (to botMessage) video cap
                liftIO $ logInfo hLogger "Video sended."
            StickerMessage' sticker -> do
                _ <-
                    liftIO $
                    sendStickerMessage hLogger tgToken (to botMessage) sticker
                liftIO $ logInfo hLogger "Sticker sended."
            VideoNoteMessage' videoNote -> do
                _ <-
                    liftIO $
                    sendVideoNoteMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        videoNote
                liftIO $ logInfo hLogger "VideoNote sended."
            VoiceMessage' voice cap -> do
                _ <-
                    liftIO $
                    sendVoiceMessage hLogger tgToken (to botMessage) voice cap
                liftIO $ logInfo hLogger "VoiceMessage sended."
            ContactMessage' contact -> do
                _ <-
                    liftIO $
                    sendContactMessage hLogger tgToken (to botMessage) contact
                liftIO $ logInfo hLogger "Contact sended."
            LocationMessage' loc -> do
                _ <-
                    liftIO $
                    sendLocationMessage hLogger tgToken (to botMessage) loc
                liftIO $ logInfo hLogger "Location sended."
            VenueMessage' venue -> do
                _ <-
                    liftIO $
                    sendVenueMessage hLogger tgToken (to botMessage) venue
                liftIO $ logInfo hLogger "Venue sended."
            _ -> return ()

tgHandler ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> Maybe (UserMessage TgMessage')
    -> Handle TgMessage' (StateT UL.RepeatsList IO)
tgHandler hLogger token helpMessage message =
    Handle
        { getMessage = return message
        , repeatsByUser = tgRepeatsByUser
        , updateRepeatsForUser = tgUpdateRepeatsForUser
        , sendAnswer = tgSendAnswer hLogger token helpMessage
        }

loopBot ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> Maybe UpdateId
    -> StateT UL.RepeatsList IO ()
loopBot hLogger token helpMessage updateId = do
    updates <- liftIO $ getUpdates token hLogger updateId
    let usersMessages =
            case updates of
                Nothing   -> []
                Just m_um -> fromTgUpdateToUserMessage <$> m_um
    let handlers = tgHandler hLogger token helpMessage
    let messages = handlers <$> usersMessages
    mapM_ echo messages
    nextUpdateId <- liftIO $ getLastUpdateId updates hLogger
    loopBot hLogger token helpMessage nextUpdateId

startBot :: LogHandle IO -> C.BotConfig -> UL.RepeatsList -> IO ()
startBot hLogger botConf list = do
    liftIO $ logInfo hLogger "New Bot Start"
    liftIO $ logInfo hLogger "Check token"
    ch <- liftIO $ getMe (TelegramToken (C.token botConf)) hLogger
    case ch of
        Nothing -> liftIO $ logError hLogger "Bad token"
        mark -> do
            liftIO $ logInfo hLogger "Good token"
            evalStateT
                (loopBot
                     hLogger
                     (TelegramToken (C.token botConf))
                     (UL.HelpMessage $ C.help botConf)
                     Nothing)
                list

tstTelegram :: IO ()
tstTelegram = do
    confBot <- C.getConfig
    case C.bot_type confBot of
        C.TelegramBot ->
            startBot (LogHandle (C.log_priority confBot) printLog) confBot []
        _ -> return ()
