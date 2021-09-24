module Telegram.TelegramHandle where

import Logger (Handle)
import qualified Telegram.API as API
import Telegram.Types
    ( Caption, StatusResult, UpdateId, TelegramToken )
import Telegram.Requests ()
import Telegram.Responses
    ( TelegramAnimation
    , TelegramAudio
    , TelegramContact
    , TelegramDocument
    , TelegramLocation
    , TelegramMessageEntity
    , TelegramPhotoSize
    , TelegramSticker
    , TelegramText
    , TelegramUpdate
    , TelegramUser
    , TelegramVenue
    , TelegramVideo
    , TelegramVideoNote
    , TelegramVoice
    )
import UsersLists ( ChatId )


data TelegramHandle m =
    TelegramHandle 
        { getMe :: Handle m -> TelegramToken -> m (Maybe TelegramUser)
        , sendKeyboard :: Handle m -> TelegramToken -> ChatId  -> m (Maybe StatusResult )
        , getUpdates :: Handle m -> TelegramToken -> Maybe UpdateId -> m (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle m -> Maybe [TelegramUpdate] -> m (Maybe UpdateId)
        , sendTextMessage :: Handle m -> TelegramToken -> ChatId -> TelegramText -> Maybe [TelegramMessageEntity] -> m (Maybe StatusResult)
        , sendAnimationMessage :: Handle m -> TelegramToken -> ChatId -> TelegramAnimation -> Maybe Caption -> m (Maybe StatusResult)
        , sendAudioMessage :: Handle m -> TelegramToken -> ChatId -> TelegramAudio -> Maybe Caption -> m (Maybe StatusResult)
        , sendDocumentMessage :: Handle m -> TelegramToken -> ChatId -> TelegramDocument -> Maybe Caption -> m (Maybe StatusResult)
        , sendPhotoMessage :: Handle m -> TelegramToken -> ChatId -> [TelegramPhotoSize] -> Maybe Caption -> m (Maybe StatusResult)
        , sendVideoMessage :: Handle m -> TelegramToken -> ChatId -> TelegramVideo -> Maybe Caption -> m (Maybe StatusResult)
        , sendStickerMessage :: Handle m -> TelegramToken -> ChatId -> TelegramSticker -> m (Maybe StatusResult)
        , sendVideoNoteMessage :: Handle m -> TelegramToken -> ChatId -> TelegramVideoNote -> m (Maybe StatusResult)
        , sendVoiceMessage :: Handle m -> TelegramToken -> ChatId -> TelegramVoice -> Maybe Caption -> m (Maybe StatusResult)
        , sendContactMessage :: Handle m -> TelegramToken -> ChatId -> TelegramContact -> m (Maybe StatusResult)
        , sendLocationMessage :: Handle m -> TelegramToken -> ChatId -> TelegramLocation -> m (Maybe StatusResult)
        , sendVenueMessage :: Handle m -> TelegramToken -> ChatId -> TelegramVenue -> m (Maybe StatusResult)
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

