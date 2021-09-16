module Vk.VkHandle where

import Logger (Handle)
import qualified Vk.API as API
import Vk.Types ( VkToken )
import Vk.Responses (VkItem, VkResponseType)

data VKHandle m =
    VKHandle
        { getLongPollServer :: Handle m -> VkToken -> m (Maybe VkResponseType)
        , getLongPollHistory :: Handle m -> VkToken -> Int -> Int -> m (Maybe VkResponseType)
        , getTsAndPts :: Handle m -> VkToken -> m (Maybe (Int, Int))
        , sendMessageText :: Handle m -> VkToken -> VkItem -> m ()
        , sendKeyboardVk :: Handle m -> VkToken -> VkItem -> m ()
        , findRepeatNumber :: [(Int, Int)] -> Int -> Int
        , sendMessageRepeatText :: Handle m -> String -> [(Int, Int)] -> VkItem -> m (Maybe ( Int, Int))
        , repeatMessage :: Handle m -> VkToken -> [(Int, Int)] -> VkItem -> m ()
        , sendMessageHelp :: Handle m-> VkToken -> String -> VkItem -> m ()
        , updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
        }

handlerVk :: VKHandle IO
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

