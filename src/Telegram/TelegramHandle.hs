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
        , sendKeyboard :: Handle -> TelegramToken -> Int -> IO (Maybe Int)
        , sendMessage :: Handle -> TelegramToken -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , findRepeatNumber :: RepeatsList -> Int -> IO RepeatsNum
        , getUpdates :: Handle -> TelegramToken -> Maybe Int -> IO (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe Int)
        , updateListUsers :: RepeatsList -> [Maybe Repeats] -> RepeatsList
        , sendTextMessage :: Handle -> TelegramToken -> Int -> TelegramText -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , sendAnimationMessage :: Handle -> TelegramToken -> Int -> TelegramAnimation -> Maybe String -> IO (Maybe Int)
        , sendAudioMessage :: Handle -> TelegramToken -> Int -> TelegramAudio -> Maybe String -> IO (Maybe Int)
        , sendDocumentMessage :: Handle -> TelegramToken -> Int -> TelegramDocument -> Maybe String -> IO (Maybe Int)
        , sendPhotoMessage :: Handle -> TelegramToken -> Int -> [TelegramPhotoSize] -> Maybe String -> IO (Maybe Int)
        , sendVideoMessage :: Handle -> TelegramToken -> Int -> TelegramVideo -> Maybe String -> IO (Maybe Int)
        , sendStickerMessage :: Handle -> TelegramToken -> Int -> TelegramSticker -> IO (Maybe Int)
        , sendVideoNoteMessage :: Handle -> TelegramToken -> Int -> TelegramVideoNote -> IO (Maybe Int)
        , sendVoiceMessage :: Handle -> TelegramToken -> Int -> TelegramVoice -> Maybe String -> IO (Maybe Int)
        , sendContactMessage :: Handle -> TelegramToken -> Int -> TelegramContact -> IO (Maybe Int)
        , sendLocationMessage :: Handle -> TelegramToken -> Int -> TelegramLocation -> IO (Maybe Int)
        , sendVenueMessage :: Handle -> TelegramToken -> Int -> TelegramVenue -> IO (Maybe Int)
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
