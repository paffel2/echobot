module Vk.VkHandle where

import           Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask),
                                       ReaderT, when)

import           Control.Monad.State  (MonadState (get, put), StateT)
import           Echo                 (BotMessage (botMessageContent, to),
                                       BotMessageContent (HelpMessage, Keyboard, PlainText, RepeatMessage),
                                       DataLoop (..), Handle (..), UserMessage)
import           Logger               (LogHandle)
import qualified UsersLists           as UL
import           Vk.API               (sendGeoVK, sendKeyboardVk,
                                       sendMessageAttachment, sendMessageHelp,
                                       sendMessageRepeatText, sendMessageText)
import           Vk.Responses         (VkMessageTypes (..))
import           Vk.Types             (Pts, Ts, VkToken)

vkRepeatsByUser ::
       UL.ChatId
    -> ReaderT (Maybe (UserMessage VkMessageTypes)) (StateT (DataLoop (Ts, Pts)) IO) (Maybe UL.RepeatsNum)
vkRepeatsByUser chatId = do
    loopInfo <- get
    return $ Just $ UL.findRepeatNumber (getRepeatsList loopInfo) chatId

vkUpdateRepeatsForUser ::
       UL.ChatId
    -> UL.RepeatsNum
    -> ReaderT (Maybe (UserMessage VkMessageTypes)) (StateT (DataLoop (Ts, Pts)) IO) ()
vkUpdateRepeatsForUser chatId repeatsNums = do
    loopInfo <- get
    let listUpdate =
            UL.updateListUsers
                (getRepeatsList loopInfo)
                [UL.Repeats chatId repeatsNums]
    put $ DataLoop listUpdate (getUpdateId loopInfo)

vkSendAnswer ::
       LogHandle IO
    -> VkToken
    -> UL.HelpMessage
    -> BotMessage VkMessageTypes
    -> ReaderT (Maybe (UserMessage VkMessageTypes)) (StateT (DataLoop (Ts, Pts)) IO) ()
vkSendAnswer hLogger vkToken helpMessage botMessage =
    case botMessageContent botMessage of
        PlainText s ->
            liftIO $ sendMessageRepeatText hLogger vkToken s (to botMessage)
        Keyboard -> liftIO $ sendKeyboardVk hLogger vkToken (to botMessage)
        HelpMessage ->
            liftIO $ sendMessageHelp hLogger vkToken helpMessage (to botMessage)
        RepeatMessage rn vmt -> repeatAnswer rn vmt
  where
    oneAnswer vmt =
        case vmt of
            VkTextMessage text ->
                liftIO $ sendMessageText hLogger vkToken (to botMessage) text
            VkGeoMessage geo Nothing ->
                liftIO $ sendGeoVK hLogger vkToken (to botMessage) geo
            VkGeoMessage geo (Just text) -> do
                liftIO $ sendMessageText hLogger vkToken (to botMessage) text
                liftIO $ sendGeoVK hLogger vkToken (to botMessage) geo
            VkWithAttachmentsMessage attachments Nothing ->
                liftIO $
                sendMessageAttachment
                    hLogger
                    vkToken
                    (to botMessage)
                    attachments
            VkWithAttachmentsMessage attachments (Just text) -> do
                liftIO $ sendMessageText hLogger vkToken (to botMessage) text
                liftIO $
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
    -> Handle VkMessageTypes (ReaderT (Maybe (UserMessage VkMessageTypes)) (StateT (DataLoop ( Ts
                                                                                             , Pts)) IO))
vkHandler hLogger token helpMessage =
    Handle
        { getMessage = ask
        , repeatsByUser = vkRepeatsByUser
        , updateRepeatsForUser = vkUpdateRepeatsForUser
        , sendAnswer = vkSendAnswer hLogger token helpMessage
        }
