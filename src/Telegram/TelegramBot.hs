{-# LANGUAGE FlexibleContexts #-}

module Telegram.TelegramBot where

import qualified Config              as C
import           Control.Monad
import           Control.Monad.State
import qualified Data.Text           as T
import           Logger
import           Logic
import           Telegram.API
import           Telegram.Impl
import           Telegram.Responses
import           Telegram.Types
import           UsersLists

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

tgRepeatsByUser :: ChatId -> StateT RepeatsList IO (Maybe RepeatsNum)
tgRepeatsByUser chatId = do
    list <- get
    return $ Just $ findRepeatNumber list chatId

tgUpdateRepeatsForUser :: ChatId -> RepeatsNum -> StateT RepeatsList IO ()
tgUpdateRepeatsForUser chatId repeatsNums = do
    list <- get
    put (updateListUsers list [Repeats chatId repeatsNums])

tgSendAnswer ::
       LogHandle IO
    -> TelegramToken
    -> BotMessage TgMessage'
    -> StateT RepeatsList IO ()
tgSendAnswer hLogger tgToken botMessage =
    case botMessageContent botMessage of
        PlainText s -> do
            _ <-
                liftIO $
                sendTextMessage hLogger tgToken (to botMessage) s Nothing
            return ()
        Keyboard -> do
            _ <- liftIO $ sendKeyboard hLogger tgToken (to botMessage)
            return ()
        RepeatMessage rn tm -> repeatAnswer rn tm
  where
    repeatAnswer rn tm =
        when (getRepeatsNum rn > 0) $ do
            oneAnswer tm
            repeatAnswer (RepeatsNum (getRepeatsNum rn - 1)) tm
    oneAnswer tm =
        case tm of
            TextMessage' text -> do
                _ <-
                    liftIO $
                    sendTextMessage hLogger tgToken (to botMessage) text Nothing
                liftIO $ logInfo hLogger "TextMessage sended."
            AnimationMessage' anim -> do
                _ <-
                    liftIO $
                    sendAnimationMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        anim
                        Nothing
                liftIO $ logInfo hLogger "Animation sended."
            AudioMessage' audio -> do
                _ <-
                    liftIO $
                    sendAudioMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        audio
                        Nothing
                liftIO $ logInfo hLogger "Audio sended."
            DocumentMessage' doc -> do
                _ <-
                    liftIO $
                    sendDocumentMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        doc
                        Nothing
                liftIO $ logInfo hLogger "Document sended."
            PhotoMessage' photo -> do
                _ <-
                    liftIO $
                    sendPhotoMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        photo
                        Nothing
                liftIO $ logInfo hLogger "Photo sended."
            VideoMessage' video -> do
                _ <-
                    liftIO $
                    sendVideoMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        video
                        Nothing
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
            VoiceMessage' voice -> do
                _ <-
                    liftIO $
                    sendVoiceMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        voice
                        Nothing
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
    -> Maybe (UserMessage TgMessage')
    -> Handle TgMessage' (StateT RepeatsList IO)
tgHandler hLogger token message =
    Handle
        { getMessage = return message
        , repeatsByUser = tgRepeatsByUser
        , updateRepeatsForUser = tgUpdateRepeatsForUser
        , sendAnswer = tgSendAnswer hLogger token
        }

loopBot ::
       LogHandle IO
    -> TelegramToken
    -> String
    -> Maybe UpdateId
    -> StateT RepeatsList IO ()
loopBot hLogger token helpMessage updateId = do
    updates <- liftIO $ getUpdates token hLogger updateId
    let usersMessages =
            case updates of
                Nothing   -> []
                Just m_um -> fromTgUpdateToUserMessage <$> m_um
    let a = tgHandler hLogger token
    let b = a <$> usersMessages
    mapM_ (echo helpMessage) b
    nextUpdateId <- liftIO $ getLastUpdateId updates hLogger
    loopBot hLogger token helpMessage nextUpdateId

startBot :: LogHandle IO -> C.BotConfig -> RepeatsList -> IO ()
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
                     (C.help botConf)
                     Nothing)
                list

tstTelegram :: IO ()
tstTelegram = do
    confBot <- C.getConfig
    case C.bot_type confBot of
        C.TelegramBot ->
            startBot (LogHandle (C.log_priority confBot) printLog) confBot []
        _ -> return ()
