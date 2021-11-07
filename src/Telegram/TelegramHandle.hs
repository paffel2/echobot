module Telegram.TelegramHandle where

import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask),
                                       ReaderT, when)
import           Control.Monad.State  (MonadState (get, put), StateT)
import           Echo                 (BotMessage (botMessageContent, to),
                                       BotMessageContent (HelpMessage, Keyboard, PlainText, RepeatMessage),
                                       DataLoop (..), Handle (..), UserMessage)
import           Logger               (LogHandle, logInfo)
import           Telegram.API         (sendAnimationMessage, sendAudioMessage,
                                       sendContactMessage, sendDocumentMessage,
                                       sendKeyboard, sendLocationMessage,
                                       sendPhotoMessage, sendStickerMessage,
                                       sendTextMessage, sendVenueMessage,
                                       sendVideoMessage, sendVideoNoteMessage,
                                       sendVoiceMessage)
import           Telegram.Responses   (TgMessage (AnimationMessage', AudioMessage, ContactMessage, DocumentMessage, LocationMessage, PhotoMessage, StickerMessage, TextMessage, VenueMessage, VideoMessage, VideoNoteMessage, VoiceMessage))
import           Telegram.Types       (TelegramToken (..), UpdateId)
import qualified UsersLists           as UL

tgRepeatsByUser ::
       UL.ChatId
    -> ReaderT (Maybe (UserMessage TgMessage)) (StateT (DataLoop UpdateId) IO) (Maybe UL.RepeatsNum)
tgRepeatsByUser chatId = do
    loopInfo <- get
    return $ Just $ UL.findRepeatNumber (getRepeatsList loopInfo) chatId

tgUpdateRepeatsForUser ::
       UL.ChatId
    -> UL.RepeatsNum
    -> ReaderT (Maybe (UserMessage TgMessage)) (StateT (DataLoop UpdateId) IO) ()
tgUpdateRepeatsForUser chatId repeatsNums = do
    loopInfo <- get
    let listUpdate =
            UL.updateListUsers
                (getRepeatsList loopInfo)
                [UL.Repeats chatId repeatsNums]
    put $ DataLoop listUpdate (getUpdateId loopInfo)

tgSendAnswer ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> BotMessage TgMessage
    -> ReaderT (Maybe (UserMessage TgMessage)) (StateT (DataLoop UpdateId) IO) ()
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
            TextMessage text entity -> do
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
            AudioMessage audio сap -> do
                _ <-
                    liftIO $
                    sendAudioMessage hLogger tgToken (to botMessage) audio сap
                liftIO $ logInfo hLogger "Audio sended."
            DocumentMessage doc cap -> do
                _ <-
                    liftIO $
                    sendDocumentMessage hLogger tgToken (to botMessage) doc cap
                liftIO $ logInfo hLogger "Document sended."
            PhotoMessage photo cap -> do
                _ <-
                    liftIO $
                    sendPhotoMessage hLogger tgToken (to botMessage) photo cap
                liftIO $ logInfo hLogger "Photo sended."
            VideoMessage video cap -> do
                _ <-
                    liftIO $
                    sendVideoMessage hLogger tgToken (to botMessage) video cap
                liftIO $ logInfo hLogger "Video sended."
            StickerMessage sticker -> do
                _ <-
                    liftIO $
                    sendStickerMessage hLogger tgToken (to botMessage) sticker
                liftIO $ logInfo hLogger "Sticker sended."
            VideoNoteMessage videoNote -> do
                _ <-
                    liftIO $
                    sendVideoNoteMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        videoNote
                liftIO $ logInfo hLogger "VideoNote sended."
            VoiceMessage voice cap -> do
                _ <-
                    liftIO $
                    sendVoiceMessage hLogger tgToken (to botMessage) voice cap
                liftIO $ logInfo hLogger "VoiceMessage sended."
            ContactMessage contact -> do
                _ <-
                    liftIO $
                    sendContactMessage hLogger tgToken (to botMessage) contact
                liftIO $ logInfo hLogger "Contact sended."
            LocationMessage loc -> do
                _ <-
                    liftIO $
                    sendLocationMessage hLogger tgToken (to botMessage) loc
                liftIO $ logInfo hLogger "Location sended."
            VenueMessage venue -> do
                _ <-
                    liftIO $
                    sendVenueMessage hLogger tgToken (to botMessage) venue
                liftIO $ logInfo hLogger "Venue sended."
            _ -> return ()

tgHandler ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> Handle TgMessage (ReaderT (Maybe (UserMessage TgMessage)) (StateT (DataLoop UpdateId) IO))
tgHandler hLogger token helpMessage =
    Handle
        { getMessage = ask
        , repeatsByUser = tgRepeatsByUser
        , updateRepeatsForUser = tgUpdateRepeatsForUser
        , sendAnswer = tgSendAnswer hLogger token helpMessage
        }
