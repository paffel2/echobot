module Vk.VkHandle where

import Logger (Handle)
import qualified Vk.API as API
import Vk.Types
    ( HelpMessage, Pts, RepeatsList, RepeatsNum, Ts, UserId, VkToken )
import Vk.Responses (VkItem, VkResponseType)

data VKHandle m =
    VKHandle
        { getLongPollServer :: Handle m -> VkToken -> m (Maybe VkResponseType)
        , getLongPollHistory :: Handle m -> VkToken -> Ts -> Pts -> m (Maybe VkResponseType)
        , getTsAndPts :: Handle m -> VkToken -> m (Maybe (Ts, Pts))
        , sendMessageText :: Handle m -> VkToken -> VkItem -> m ()
        , sendKeyboardVk :: Handle m -> VkToken -> VkItem -> m ()
        , findRepeatNumber :: RepeatsList  -> UserId -> RepeatsNum 
        , sendMessageRepeatText :: Handle m -> VkToken  -> RepeatsList -> VkItem -> m (Maybe ( UserId, RepeatsNum))
        , repeatMessage :: Handle m -> VkToken -> RepeatsList -> VkItem -> m ()
        , sendMessageHelp :: Handle m-> VkToken -> HelpMessage  -> VkItem -> m ()
        , updateListUsers :: RepeatsList -> [Maybe (UserId, RepeatsNum)] -> RepeatsList
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

