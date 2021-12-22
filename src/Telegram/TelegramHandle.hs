module Telegram.TelegramHandle where

import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask),
                                       ReaderT, replicateM_)
import           Control.Monad.State  (StateT)
import           Data.Functor         (void)
import           Echo                 (BotMessage (botMessageContent, to),
                                       BotMessageContent (Keyboard, PlainText, RepeatMessage),
                                       Handle (..), UserMessage)
import           Logger               (LogHandle, logInfo)
import           Telegram.API         (sendAnimationMessage, sendAudioMessage,
                                       sendContactMessage, sendDocumentMessage,
                                       sendKeyboard, sendLocationMessage,
                                       sendPhotoMessage, sendStickerMessage,
                                       sendTextMessage, sendVenueMessage,
                                       sendVideoMessage, sendVideoNoteMessage,
                                       sendVoiceMessage)
import           Telegram.Responses   (TgMessage (AnimationMessage, AudioMessage, ContactMessage, DocumentMessage, LocationMessage, PhotoMessage, StickerMessage, TextMessage, VenueMessage, VideoMessage, VideoNoteMessage, VoiceMessage))
import           Telegram.Types       (TelegramToken (..), UpdateId)
import qualified UsersLists           as UL

tgSendAnswer ::
       LogHandle IO
    -> TelegramToken
    -> BotMessage TgMessage
    -> ReaderT (UserMessage TgMessage) (StateT (UL.DataLoop UpdateId) IO) ()
tgSendAnswer hLogger tgToken botMessage =
    liftIO $
    case botMessageContent botMessage of
        PlainText s -> do
            void $ sendTextMessage hLogger tgToken (to botMessage) s Nothing
        Keyboard -> do
            void $ sendKeyboard hLogger tgToken (to botMessage)
        RepeatMessage rn tm -> repeatAnswer rn tm
  where
    repeatAnswer (UL.RepeatsNum n) tm =
        case tm of
            TextMessage text entity -> do
                replicateM_ n $
                    sendTextMessage hLogger tgToken (to botMessage) text entity
                logInfo hLogger "TextMessage sended."
            AnimationMessage anim cap -> do
                replicateM_ n $
                    sendAnimationMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        anim
                        cap
                logInfo hLogger "Animation sended."
            AudioMessage audio сap -> do
                replicateM_ n $
                    sendAudioMessage hLogger tgToken (to botMessage) audio сap
                logInfo hLogger "Audio sended."
            DocumentMessage doc cap -> do
                replicateM_ n $
                    sendDocumentMessage hLogger tgToken (to botMessage) doc cap
                logInfo hLogger "Document sended."
            PhotoMessage photo cap -> do
                replicateM_ n $
                    sendPhotoMessage hLogger tgToken (to botMessage) photo cap
                logInfo hLogger "Photo sended."
            VideoMessage video cap -> do
                replicateM_ n $
                    sendVideoMessage hLogger tgToken (to botMessage) video cap
                logInfo hLogger "Video sended."
            StickerMessage sticker -> do
                replicateM_ n $
                    sendStickerMessage hLogger tgToken (to botMessage) sticker
                logInfo hLogger "Sticker sended."
            VideoNoteMessage videoNote -> do
                replicateM_ n $
                    sendVideoNoteMessage
                        hLogger
                        tgToken
                        (to botMessage)
                        videoNote
                logInfo hLogger "VideoNote sended."
            VoiceMessage voice cap -> do
                replicateM_ n $
                    sendVoiceMessage hLogger tgToken (to botMessage) voice cap
                logInfo hLogger "VoiceMessage sended."
            ContactMessage contact -> do
                replicateM_ n $
                    sendContactMessage hLogger tgToken (to botMessage) contact
                logInfo hLogger "Contact sended."
            LocationMessage loc -> do
                replicateM_ n $
                    sendLocationMessage hLogger tgToken (to botMessage) loc
                logInfo hLogger "Location sended."
            VenueMessage venue -> do
                replicateM_ n $
                    sendVenueMessage hLogger tgToken (to botMessage) venue
                liftIO $ logInfo hLogger "Venue sended."
            _ -> return ()

tgHandler ::
       LogHandle IO
    -> TelegramToken
    -> UL.HelpMessage
    -> Handle TgMessage (ReaderT (UserMessage TgMessage) (StateT (UL.DataLoop UpdateId) IO))
tgHandler hLogger token helpMessageFromConfig =
    Handle
        { getMessage = ask
        , repeatsByUser = UL.repeatsByUser
        , updateRepeatsForUser = UL.updateRepeatsForUser
        , sendAnswer = tgSendAnswer hLogger token
        , helpMessage = UL.getHelpMessage helpMessageFromConfig
        }
