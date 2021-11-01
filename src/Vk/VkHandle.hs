module Vk.VkHandle where

import           Logger       (LogHandle)
import           UsersLists   (Repeats, RepeatsList)
import qualified Vk.API       as API
import           Vk.Responses (VkItem, VkResponseType)
import           Vk.Types     (HelpMessage, Pts, Ts, VkToken)

data VKHandle m =
    VKHandle
        { getLongPollServer :: LogHandle m -> VkToken -> m (Maybe VkResponseType)
        , getLongPollHistory :: LogHandle m -> VkToken -> Ts -> Pts -> m (Maybe VkResponseType)
        , getTsAndPts :: LogHandle m -> VkToken -> m (Maybe (Ts, Pts))
        , sendMessageText :: LogHandle m -> VkToken -> VkItem -> m ()
        , sendKeyboardVk :: LogHandle m -> VkToken -> VkItem -> m ()
        , sendMessageRepeatText :: LogHandle m -> VkToken -> RepeatsList -> VkItem -> m (Maybe Repeats)
        , repeatMessage :: LogHandle m -> VkToken -> RepeatsList -> VkItem -> m ()
        , sendMessageHelp :: LogHandle m -> VkToken -> HelpMessage -> VkItem -> m ()
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
