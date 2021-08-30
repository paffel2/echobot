module Vk.VkHandle where

import Logger (Handle)
import qualified Vk.API as API
import Vk.BuildRequests (VkToken)
import Vk.Responses (VkItem, VkResponseType)

data VKHandle =
    VKHandle
        { getLongPollServer :: Handle -> VkToken -> IO (Maybe VkResponseType)
        , getLongPollHistory :: Handle -> VkToken -> Int -> Int -> IO (Maybe VkResponseType)
        , getTsAndPts :: Handle -> VkToken -> IO (Maybe (Int, Int))
        , sendMessageText :: Handle -> VkToken -> VkItem -> IO ()
        , sendKeyboardVk :: Handle -> VkToken -> VkItem -> IO ()
        , findRepeatNumber :: [(Int, Int)] -> Int -> Int
        , sendMessageRepeatText :: Handle -> String -> [(Int, Int)] -> VkItem -> IO (Maybe ( Int
                                                                                           , Int))
        , repeatMessage :: Handle -> VkToken -> [(Int, Int)] -> VkItem -> IO ()
        , sendMessageHelp :: Handle -> VkToken -> String -> VkItem -> IO ()
        , updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
        }

handlerVk :: VKHandle
handlerVk =
    VKHandle
        API.getLongPollServer
        API.getLongPollHistory
        API.getTsAndPts
        API.sendMessageText
        API.sendKeyboardVk
        API.findRepeatNumber
        API.sendMessageRepeatText
        API.repeatMessage
        API.sendMessageHelp
        API.updateListUsers
