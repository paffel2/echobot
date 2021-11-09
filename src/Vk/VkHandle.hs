module Vk.VkHandle where

import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask),
                                       ReaderT, when)

import           Control.Monad.State  (StateT)
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
        RepeatMessage rn vmt -> repeatAnswer rn vmt
  where
    oneAnswer vmt =
        case vmt of
            VkTextMessage text ->
                sendMessageText hLogger vkToken (to botMessage) text
            VkGeoMessage geo Nothing ->
                sendGeoVK hLogger vkToken (to botMessage) geo
            VkGeoMessage geo (Just text) -> do
                sendMessageText hLogger vkToken (to botMessage) text
                sendGeoVK hLogger vkToken (to botMessage) geo
            VkWithAttachmentsMessage attachments Nothing ->
                sendMessageAttachment
                    hLogger
                    vkToken
                    (to botMessage)
                    attachments
            VkWithAttachmentsMessage attachments (Just text) -> do
                sendMessageText hLogger vkToken (to botMessage) text
                sendMessageAttachment
                    hLogger
                    vkToken
                    (to botMessage)
                    attachments
    repeatAnswer (UL.RepeatsNum n) vmt = do
        when (n > 0) $ do
            oneAnswer vmt
            repeatAnswer (UL.RepeatsNum (n - 1)) vmt

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
