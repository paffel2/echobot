module Telegram.Echo where

import Logger (Handle, logDebug, logError)
import Telegram.Impl ( telegramMessageToTgMessage ) 
import Telegram.Responses
    ( TelegramCallbackQuery(TelegramCallbackQuery),
      TelegramMessageEntity,
      TgMessage(..),
      TelegramCommand(..),
      TelegramMessage(telegramMessageChat, telegramMessageEntities,
                      telegramMessageCaption),
      TelegramChat(telegramChatId),
      TelegramUser(telegramUserId),
      TelegramUpdate(TelegramUpdate) )

import Telegram.TelegramHandle
    ( TelegramHandle(getLastUpdateId, getUpdates, sendAnimationMessage,
               sendAudioMessage, sendContactMessage, sendDocumentMessage,
               sendKeyboard, sendLocationMessage, sendPhotoMessage,
               sendStickerMessage, sendTextMessage, sendVenueMessage,
               sendVideoMessage, sendVideoNoteMessage, sendVoiceMessage)
    )

import Telegram.Types
    ( Caption,
      HelpMessage(help_mess),
      StatusResult,
      TelegramToken,
      UpdateId )
import Data.Maybe (catMaybes)
import UsersLists
    ( findRepeatNumber,
      updateListUsers,
      ChatId,
      Repeats(Repeats),
      RepeatsList,
      RepeatsNum(..) )



echo ::
       Monad m
    => Handle m
    -> TelegramHandle m
    -> TelegramToken
    -> Maybe UpdateId
    -> HelpMessage
    -> RepeatsList
    -> m (Maybe UpdateId,RepeatsList)
echo hLogger' hTelegram' tgtoken' updateId help_message' listOfUsers = do
    updates <- getUpdates hTelegram' hLogger' tgtoken' updateId
    listOfUsersUpd <-
        answers hLogger' hTelegram' help_message' tgtoken' updates listOfUsers
    let newListOfUsers = updateListUsers listOfUsers listOfUsersUpd
    nextUpdateID <- getLastUpdateId hTelegram' hLogger' updates
    return (nextUpdateID,newListOfUsers)
  where
    answers hLogger hTelegram help_message tgtoken (Just upd) list =
        catMaybes <$>
        mapM (answer hLogger hTelegram help_message tgtoken list) upd
    answers hLogger _ _ _ _ _ = do
        logError hLogger "Something wrong"
        return []
    answer hLogger hTelegram help_message tgtoken list (TelegramUpdate _ (Just message) _) = do
        let tg_message = telegramMessageToTgMessage message
        case tg_message of
            Nothing -> return Nothing
            Just tm -> do
                _ <-
                    repeatSendMessage
                        hLogger
                        hTelegram
                        (findRepeatNumber list chatId)
                        tgtoken
                        chatId
                        tm
                        entities
                        cap
                        help_message
                return Nothing
      where
        chatId = telegramChatId $ telegramMessageChat message
        entities = telegramMessageEntities message
        cap = telegramMessageCaption message
    answer hLogger hTelegram _ tgtoken _ (TelegramUpdate _ _ (Just (TelegramCallbackQuery _ user (Just _) _ (Just dat)))) = do
        status <- sendTextMessage hTelegram hLogger tgtoken chatId text Nothing
        case status of
            Nothing -> do
                logError hLogger "New repeats num not updated"
                return Nothing
            Just _ -> do
                logDebug hLogger "New repeats num updated"
                return $ Just $ Repeats chatId dat
      where
        chatId = telegramUserId user
        text = "Number of reapeting " ++ (show . repeats_num' $ dat)
    answer _ _ _ _ _ _ = return Nothing

sendAnswer ::
       Monad m
    => Handle m
    -> TelegramHandle m
    -> TelegramToken
    -> ChatId
    -> TgMessage
    -> Maybe [TelegramMessageEntity]
    -> Maybe Caption
    -> m (Maybe StatusResult)
sendAnswer hLogger hTelegram tgtoken chatId tg_message ent cap =
    case tg_message of
        TextMessage telegram_text ->
            sendTextMessage hTelegram hLogger tgtoken chatId telegram_text ent
        AnimationMessage telegram_animation ->
            sendAnimationMessage
                hTelegram
                hLogger
                tgtoken
                chatId
                telegram_animation
                cap
        AudioMessage telegram_audio ->
            sendAudioMessage hTelegram hLogger tgtoken chatId telegram_audio cap
        DocumentMessage telegram_document ->
            sendDocumentMessage
                hTelegram
                hLogger
                tgtoken
                chatId
                telegram_document
                cap
        PhotoMessage telegram_photosizes ->
            sendPhotoMessage
                hTelegram
                hLogger
                tgtoken
                chatId
                telegram_photosizes
                cap
        VideoMessage telegram_video ->
            sendVideoMessage hTelegram hLogger tgtoken chatId telegram_video cap
        StickerMessage telegram_sticker ->
            sendStickerMessage hTelegram hLogger tgtoken chatId telegram_sticker
        VideoNoteMessage telegram_videonote ->
            sendVideoNoteMessage
                hTelegram
                hLogger
                tgtoken
                chatId
                telegram_videonote
        VoiceMessage telegram_voice ->
            sendVoiceMessage hTelegram hLogger tgtoken chatId telegram_voice cap
        ContactMessage telegram_contact ->
            sendContactMessage hTelegram hLogger tgtoken chatId telegram_contact
        LocationMessage telegram_location ->
            sendLocationMessage
                hTelegram
                hLogger
                tgtoken
                chatId
                telegram_location
        VenueMessage telegram_venue ->
            sendVenueMessage hTelegram hLogger tgtoken chatId telegram_venue
        _ -> return Nothing

repeatSendMessage ::
       Monad m
    => Handle m
    -> TelegramHandle m
    -> RepeatsNum
    -> TelegramToken
    -> ChatId
    -> TgMessage
    -> Maybe [TelegramMessageEntity]
    -> Maybe Caption
    -> HelpMessage
    -> m ()
repeatSendMessage hLogger hTelegram n tgtoken chatId tg_message entities cap help_message = do
    case tg_message of
        CommandMessage telegram_command -> do
            _ <-
                sendServiceMessage
                    hLogger
                    hTelegram
                    tgtoken
                    chatId
                    telegram_command
                    help_message
            return ()
        _ -> repeatedMessages n
  where
    repeatedMessages (RepeatsNum n')
        | n' > 0 = do
            status <-
                sendAnswer
                    hLogger
                    hTelegram
                    tgtoken
                    chatId
                    tg_message
                    entities
                    cap
            case status of
                Nothing -> do
                    logError hLogger "Message not send"
                    return ()
                Just _ -> repeatedMessages $ RepeatsNum (n' - 1)
        | otherwise = do
            logDebug hLogger "All messages sended"
            return ()


sendServiceMessage ::
       Monad m
    => Handle m
    -> TelegramHandle m
    -> TelegramToken
    -> ChatId
    -> TelegramCommand
    -> HelpMessage
    -> m (Maybe StatusResult)
sendServiceMessage hLogger hTelegram tgtoken chatId Repeat _ =
    sendKeyboard hTelegram hLogger tgtoken chatId
sendServiceMessage hLogger hTelegram tgtoken chatId Help help_message =
    sendTextMessage hTelegram hLogger tgtoken chatId (help_mess help_message) Nothing

