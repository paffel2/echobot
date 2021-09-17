module Vk.VkHandle where

import Logger (Handle)
import UsersLists (RepeatsList, RepeatsNum)
import qualified Vk.API as API
import Vk.Responses (VkItem, VkResponseType)
import Vk.Types (HelpMessage, Pts, Ts, UserId, VkToken)

data VKHandle m =
    VKHandle
        { getLongPollServer :: Handle m -> VkToken -> m (Maybe VkResponseType)
        , getLongPollHistory :: Handle m -> VkToken -> Ts -> Pts -> m (Maybe VkResponseType)
        , getTsAndPts :: Handle m -> VkToken -> m (Maybe (Ts, Pts))
        , sendMessageText :: Handle m -> VkToken -> VkItem -> m ()
        , sendKeyboardVk :: Handle m -> VkToken -> VkItem -> m ()
        , sendMessageRepeatText :: Handle m -> VkToken -> RepeatsList -> VkItem -> m (Maybe ( UserId
                                                                                            , RepeatsNum))
        , repeatMessage :: Handle m -> VkToken -> RepeatsList -> VkItem -> m ()
        , sendMessageHelp :: Handle m -> VkToken -> HelpMessage -> VkItem -> m ()
        }

handlerVk :: VKHandle IO
handlerVk =
    VKHandle
        API.getLongPollServer
        API.getLongPollHistory
        API.getTsAndPts
        API.sendMessageText
        API.sendKeyboardVk
        API.sendMessageRepeatText
        API.repeatMessage
        API.sendMessageHelp
