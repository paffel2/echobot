module Telegram.TelegramHandle where

import Logger (Handle)
import qualified Telegram.API as API
import Telegram.Types
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

data TelegramHandle =
    TelegramHandle
        { getMe :: Handle -> TelegramToken -> IO (Maybe TelegramUser)
        , sendKeyboard :: Handle -> TelegramToken -> ChatId -> IO (Maybe StatusResult)
        , sendMessage :: Handle -> TelegramToken -> ChatId -> String -> Maybe [TelegramMessageEntity] -> IO (Maybe StatusResult)
        , findRepeatNumber :: RepeatsList -> ChatId -> IO RepeatsNum
        , getUpdates :: Handle -> TelegramToken -> Maybe UpdateId -> IO (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe UpdateId)
        , updateListUsers :: RepeatsList -> [Maybe Repeats] -> RepeatsList
        , sendTextMessage :: Handle -> TelegramToken -> ChatId -> TelegramText -> Maybe [TelegramMessageEntity] -> IO (Maybe StatusResult)
        , sendAnimationMessage :: Handle -> TelegramToken -> ChatId -> TelegramAnimation -> Maybe Caption -> IO (Maybe StatusResult)
        , sendAudioMessage :: Handle -> TelegramToken -> ChatId -> TelegramAudio -> Maybe Caption -> IO (Maybe StatusResult)
        , sendDocumentMessage :: Handle -> TelegramToken -> ChatId -> TelegramDocument -> Maybe Caption -> IO (Maybe StatusResult)
        , sendPhotoMessage :: Handle -> TelegramToken -> ChatId -> [TelegramPhotoSize] -> Maybe Caption -> IO (Maybe StatusResult)
        , sendVideoMessage :: Handle -> TelegramToken -> ChatId -> TelegramVideo -> Maybe Caption -> IO (Maybe StatusResult)
        , sendStickerMessage :: Handle -> TelegramToken -> ChatId -> TelegramSticker -> IO (Maybe StatusResult)
        , sendVideoNoteMessage :: Handle -> TelegramToken -> ChatId -> TelegramVideoNote -> IO (Maybe StatusResult)
        , sendVoiceMessage :: Handle -> TelegramToken -> ChatId -> TelegramVoice -> Maybe Caption -> IO (Maybe StatusResult)
        , sendContactMessage :: Handle -> TelegramToken -> ChatId -> TelegramContact -> IO (Maybe StatusResult)
        , sendLocationMessage :: Handle -> TelegramToken -> ChatId -> TelegramLocation -> IO (Maybe StatusResult)
        , sendVenueMessage :: Handle -> TelegramToken -> ChatId -> TelegramVenue -> IO (Maybe StatusResult)
        }

telegramHandler :: TelegramHandle
telegramHandler =
    TelegramHandle
        API.getMe
        API.sendKeyboard
        API.sendMessage
        API.findRepeatNumber
        API.getUpdates
        API.getLastUpdateId
        API.updateListUsers
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
