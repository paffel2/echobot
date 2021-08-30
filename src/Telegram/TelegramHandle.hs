module Telegram.TelegramHandle where

import Logger (Handle)
import qualified Telegram.API as API
import Telegram.BuildRequest (TelegramToken)
import Telegram.Requests ()
import Telegram.Responses (TelegramMessageEntity, TelegramUpdate, TelegramUser)

data TelegramHandle =
    TelegramHandle
        { getMe :: Handle -> TelegramToken -> IO (Maybe TelegramUser)
        , sendKeyboard :: Handle -> String -> Int -> IO (Maybe Int)
        , repeatSendMessage :: Handle -> Int -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , repeatSendAnimation :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendAudio :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendDocument :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendPhoto :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendVideo :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendSticker :: Handle -> Int -> String -> Int -> String -> IO (Maybe Int)
        , repeatSendVideoNote :: Handle -> Int -> String -> Int -> String -> IO (Maybe Int)
        , repeatSendVoice :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
        , repeatSendContact :: Handle -> Int -> String -> Int -> String -> String -> Maybe String -> Maybe String -> IO (Maybe Int)
        , repeatSendLocation :: Handle -> Int -> String -> Int -> Double -> Double -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Maybe Int)
        , repeatSendVenue :: Handle -> Int -> String -> Int -> Double -> Double -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe Int)
        , sendMessage :: Handle -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Maybe Int)
        , findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
        , getUpdates :: Handle -> TelegramToken -> Maybe Int -> IO (Maybe [TelegramUpdate])
        , getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe Int)
        , updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
        }

telegramHandler :: TelegramHandle
telegramHandler =
    TelegramHandle
        API.getMe
        API.sendKeyboard
        API.repeatSendMessage
        API.repeatSendAnimation
        API.repeatSendAudio
        API.repeatSendDocument
        API.repeatSendPhoto
        API.repeatSendVideo
        API.repeatSendSticker
        API.repeatSendVideoNote
        API.repeatSendVoice
        API.repeatSendContact
        API.repeatSendLocation
        API.repeatSendVenue
        API.sendMessage
        API.findRepeatNumber
        API.getUpdates
        API.getLastUpdateId
        API.updateListUsers
