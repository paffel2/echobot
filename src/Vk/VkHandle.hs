module Vk.VkHandle where

import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask),
                                       ReaderT)

import           Control.Monad.State  (StateT, replicateM_)
import           Echo                 (BotMessage (botMessageContent, to),
                                       BotMessageContent (Keyboard, PlainText, RepeatMessage),
                                       Handle (..), UserMessage)
import           Logger               (LogHandle)
import qualified UsersLists           as UL
import           Vk.API               (sendGeoVK, sendKeyboardVk,
                                       sendMessageAttachment,
                                       sendMessageRepeatText, sendMessageText)
import           Vk.Responses         (VkMessageTypes (..))
import           Vk.Types             (Pts, Ts, VkToken)

vkSendAnswer ::
       LogHandle IO
    -> VkToken
    -> BotMessage VkMessageTypes
    -> ReaderT (UserMessage VkMessageTypes) (StateT (UL.DataLoop (Ts, Pts)) IO) ()
vkSendAnswer hLogger vkToken botMessage =
    liftIO $
    case botMessageContent botMessage of
        PlainText s -> sendMessageRepeatText hLogger vkToken s (to botMessage)
        Keyboard -> sendKeyboardVk hLogger vkToken (to botMessage)
        RepeatMessage rn vmt -> sendAnswers rn vmt
  where
    sendAnswers (UL.RepeatsNum n) vmt =
        case vmt of
            VkTextMessage text ->
                replicateM_ n $
                sendMessageText hLogger vkToken (to botMessage) text
            VkGeoMessage geo Nothing ->
                replicateM_ n $ sendGeoVK hLogger vkToken (to botMessage) geo
            VkGeoMessage geo (Just text) ->
                replicateM_
                    n
                    (do sendMessageText hLogger vkToken (to botMessage) text
                        sendGeoVK hLogger vkToken (to botMessage) geo)
            VkWithAttachmentsMessage attachments Nothing ->
                replicateM_ n $
                sendMessageAttachment
                    hLogger
                    vkToken
                    (to botMessage)
                    attachments
            VkWithAttachmentsMessage attachments (Just text) ->
                replicateM_
                    n
                    (do sendMessageText hLogger vkToken (to botMessage) text
                        sendMessageAttachment
                            hLogger
                            vkToken
                            (to botMessage)
                            attachments)

vkHandler ::
       LogHandle IO
    -> VkToken
    -> UL.HelpMessage
    -> Handle VkMessageTypes (ReaderT (UserMessage VkMessageTypes) (StateT (UL.DataLoop ( Ts
                                                                                        , Pts)) IO))
vkHandler hLogger token helpMessageFromConfig =
    Handle
        { getMessage = ask
        , repeatsByUser = UL.repeatsByUser
        , updateRepeatsForUser = UL.updateRepeatsForUser
        , sendAnswer = vkSendAnswer hLogger token
        , helpMessage = UL.getHelpMessage helpMessageFromConfig
        }
