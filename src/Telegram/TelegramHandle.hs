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

data TelegramHandle m =
    TelegramHandle 
        { getMe :: Handle m -> TelegramToken -> m (Maybe TelegramUser)
        , sendKeyboard :: Handle m -> String -> Int -> m (Maybe Int)
        , sendMessage :: Handle m -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> m (Maybe Int)
        , findRepeatNumber :: [(Int, Int)] -> Int -> m Int
        , getUpdates :: Handle m -> TelegramToken -> Maybe Int -> m (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle m -> Maybe [TelegramUpdate] -> m (Maybe Int)
        , updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
        , sendTextMessage :: Handle m -> String -> Int -> TelegramText -> Maybe [TelegramMessageEntity] -> m (Maybe Int)
        , sendAnimationMessage :: Handle m -> String -> Int -> TelegramAnimation -> Maybe String -> m (Maybe Int)
        , sendAudioMessage :: Handle m -> String -> Int -> TelegramAudio -> Maybe String -> m (Maybe Int)
        , sendDocumentMessage :: Handle m -> String -> Int -> TelegramDocument -> Maybe String -> m (Maybe Int)
        , sendPhotoMessage :: Handle m -> String -> Int -> [TelegramPhotoSize] -> Maybe String -> m (Maybe Int)
        , sendVideoMessage :: Handle m -> String -> Int -> TelegramVideo -> Maybe String -> m (Maybe Int)
        , sendStickerMessage :: Handle m -> String -> Int -> TelegramSticker -> m (Maybe Int)
        , sendVideoNoteMessage :: Handle m -> String -> Int -> TelegramVideoNote -> m (Maybe Int)
        , sendVoiceMessage :: Handle m -> String -> Int -> TelegramVoice -> Maybe String -> m (Maybe Int)
        , sendContactMessage :: Handle m -> String -> Int -> TelegramContact -> m (Maybe Int)
        , sendLocationMessage :: Handle m -> String -> Int -> TelegramLocation -> m (Maybe Int)
        , sendVenueMessage :: Handle m -> String -> Int -> TelegramVenue -> m (Maybe Int)
        }

telegramHandler :: TelegramHandle IO
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

{-telegramHandler' :: TelegramHandle m
telegramHandler' =
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
        API.sendVenueMessage-}


{-data TelegramHandle =
    TelegramHandle
        { getMe :: Handle -> TelegramToken -> IO (Maybe TelegramUser)
        , sendKeyboard :: Handle -> String -> Int -> IO (Maybe Int)
        , sendMessage :: Handle -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
        , getUpdates :: Handle -> TelegramToken -> Maybe Int -> IO (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe Int)
        , updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
        , sendTextMessage :: Handle -> String -> Int -> TelegramText -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , sendAnimationMessage :: Handle -> String -> Int -> TelegramAnimation -> Maybe String -> IO (Maybe Int)
        , sendAudioMessage :: Handle -> String -> Int -> TelegramAudio -> Maybe String -> IO (Maybe Int)
        , sendDocumentMessage :: Handle -> String -> Int -> TelegramDocument -> Maybe String -> IO (Maybe Int)
        , sendPhotoMessage :: Handle -> String -> Int -> [TelegramPhotoSize] -> Maybe String -> IO (Maybe Int)
        , sendVideoMessage :: Handle -> String -> Int -> TelegramVideo -> Maybe String -> IO (Maybe Int)
        , sendStickerMessage :: Handle -> String -> Int -> TelegramSticker -> IO (Maybe Int)
        , sendVideoNoteMessage :: Handle -> String -> Int -> TelegramVideoNote -> IO (Maybe Int)
        , sendVoiceMessage :: Handle -> String -> Int -> TelegramVoice -> Maybe String -> IO (Maybe Int)
        , sendContactMessage :: Handle -> String -> Int -> TelegramContact -> IO (Maybe Int)
        , sendLocationMessage :: Handle -> String -> Int -> TelegramLocation -> IO (Maybe Int)
        , sendVenueMessage :: Handle -> String -> Int -> TelegramVenue -> IO (Maybe Int)
        }-}