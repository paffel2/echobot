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
        , sendKeyboard :: Handle m -> TelegramToken -> ChatId  -> m (Maybe StatusResult )
        , sendMessage :: Handle m -> TelegramToken -> ChatId -> TelegramText -> Maybe [TelegramMessageEntity] -> m (Maybe StatusResult)
        , findRepeatNumber :: RepeatsList  -> ChatId -> m RepeatsNum
        , getUpdates :: Handle m -> TelegramToken -> Maybe UpdateId -> m (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle m -> Maybe [TelegramUpdate] -> m (Maybe UpdateId)
        , updateListUsers :: RepeatsList -> [Maybe (ChatId, RepeatsNum)] -> RepeatsList
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