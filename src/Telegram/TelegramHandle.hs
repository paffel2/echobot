module Telegram.TelegramHandle where

import           Logger             (LogHandle)
import qualified Telegram.API       as API
import           Telegram.Requests  ()
import           Telegram.Responses (TelegramAnimation, TelegramAudio,
                                     TelegramContact, TelegramDocument,
                                     TelegramLocation, TelegramMessageEntity,
                                     TelegramPhotoSize, TelegramSticker,
                                     TelegramText, TelegramUpdate, TelegramUser,
                                     TelegramVenue, TelegramVideo,
                                     TelegramVideoNote, TelegramVoice)
import           Telegram.Types     (Caption, StatusResult, TelegramToken,
                                     UpdateId)
import           UsersLists         (ChatId)

data TelegramHandle m =
    TelegramHandle
        { getMe :: LogHandle m -> TelegramToken -> m (Maybe TelegramUser)
        , sendKeyboard :: LogHandle m -> TelegramToken -> ChatId -> m (Maybe StatusResult)
        , getUpdates :: LogHandle m -> TelegramToken -> Maybe UpdateId -> m (Maybe [TelegramUpdate])
        , getLastUpdateId :: LogHandle m -> Maybe [TelegramUpdate] -> m (Maybe UpdateId)
        , sendTextMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramText -> Maybe [TelegramMessageEntity] -> m (Maybe StatusResult)
        , sendAnimationMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramAnimation -> Maybe Caption -> m (Maybe StatusResult)
        , sendAudioMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramAudio -> Maybe Caption -> m (Maybe StatusResult)
        , sendDocumentMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramDocument -> Maybe Caption -> m (Maybe StatusResult)
        , sendPhotoMessage :: LogHandle m -> TelegramToken -> ChatId -> [TelegramPhotoSize] -> Maybe Caption -> m (Maybe StatusResult)
        , sendVideoMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramVideo -> Maybe Caption -> m (Maybe StatusResult)
        , sendStickerMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramSticker -> m (Maybe StatusResult)
        , sendVideoNoteMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramVideoNote -> m (Maybe StatusResult)
        , sendVoiceMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramVoice -> Maybe Caption -> m (Maybe StatusResult)
        , sendContactMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramContact -> m (Maybe StatusResult)
        , sendLocationMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramLocation -> m (Maybe StatusResult)
        , sendVenueMessage :: LogHandle m -> TelegramToken -> ChatId -> TelegramVenue -> m (Maybe StatusResult)
        }

telegramHandler :: TelegramHandle IO
telegramHandler =
    TelegramHandle
        API.getMe
        API.sendKeyboard
        API.getUpdates
        API.getLastUpdateId
        API.sendTextMessage
        API.sendAnimationMessage
        API.sendAudioMessage
        API.sendDocumentMessage
        API.sendPhotoMessage
        API.sendVideoMessage
        API.sendStickerMessage
        API.sendVideoNoteMessage
        API.sendVoiceMessage
        API.sendContactMessage
        API.sendLocationMessage
        API.sendVenueMessage
