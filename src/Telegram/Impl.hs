{-# LANGUAGE NamedFieldPuns #-}

module Telegram.Impl where

import           Data.Maybe         (fromJust, isJust)
import qualified Logic              as L
import           Telegram.Responses (TelegramCallbackQuery (..),
                                     TelegramChat (..),
                                     TelegramCommand (Help, Repeat),
                                     TelegramCommand' (..),
                                     TelegramMessage (..), TelegramUpdate (..),
                                     TelegramUser (..), TgMessage (..),
                                     TgMessage' (..))

telegramMessageToTgMessage :: TelegramMessage -> Maybe TgMessage
telegramMessageToTgMessage telegram_message
    | telegramMessageText telegram_message == Just "/repeat" =
        Just $ CommandMessage Repeat
    | telegramMessageText telegram_message == Just "/help" =
        Just $ CommandMessage Help
    | isJust $ telegramMessageText telegram_message =
        TextMessage <$> telegramMessageText telegram_message
    | isJust $ telegramMessageAnimation telegram_message =
        AnimationMessage <$> telegramMessageAnimation telegram_message
    | isJust $ telegramMessageAudio telegram_message =
        AudioMessage <$> telegramMessageAudio telegram_message
    | isJust $ telegramMessageDocument telegram_message =
        DocumentMessage <$> telegramMessageDocument telegram_message
    | isJust $ telegramMessagePhoto telegram_message =
        PhotoMessage <$> telegramMessagePhoto telegram_message
    | isJust $ telegramMessageVideo telegram_message =
        VideoMessage <$> telegramMessageVideo telegram_message
    | isJust $ telegramMessageSticker telegram_message =
        StickerMessage <$> telegramMessageSticker telegram_message
    | isJust $ telegramMessageVideoNote telegram_message =
        VideoNoteMessage <$> telegramMessageVideoNote telegram_message
    | isJust $ telegramMessageVoice telegram_message =
        VoiceMessage <$> telegramMessageVoice telegram_message
    | isJust $ telegramMessageContact telegram_message =
        ContactMessage <$> telegramMessageContact telegram_message
    | isJust $ telegramMessageLocation telegram_message =
        LocationMessage <$> telegramMessageLocation telegram_message
    | isJust $ telegramMessageVenue telegram_message =
        VenueMessage <$> telegramMessageVenue telegram_message
    | otherwise = Nothing

fromTgUpdateToUserMessage :: TelegramUpdate -> Maybe (L.UserMessage TgMessage')
fromTgUpdateToUserMessage TelegramUpdate {telegramUpdateCallbackQuery = (Just TelegramCallbackQuery { telegramCallbackQueryData = Just n
                                                                                                    , telegramCallbackQueryMessage = Just _
                                                                                                    , telegramCallbackQueryFrom = user
                                                                                                    })} =
    Just $ L.UserMessage (telegramUserId user) (L.CommandMessage $ L.Repeat n)
fromTgUpdateToUserMessage TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                         , telegramUpdateMessage = (Just telegramMessage@TelegramMessage {telegramMessageFrom = Just user})
                                         }
    | telegramMessageText telegramMessage == Just "/repeat" =
        Just $
        L.UserMessage (telegramUserId user) (L.CommandMessage L.ChoicesRequest)
    | telegramMessageText telegramMessage == Just "/help" =
        Just $ L.UserMessage (telegramUserId user) (L.CommandMessage L.Help)
    | isJust $ telegramMessageText telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             TextMessage'
                 (fromJust $ telegramMessageText telegramMessage)
                 (telegramMessageEntities telegramMessage))
        --TextMessage' <$> telegramMessageText telegramMessage
    | isJust $ telegramMessageAnimation telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             AnimationMessage'
                 (fromJust $ telegramMessageAnimation telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --AnimationMessage' <$> telegramMessageAnimation telegramMessage
    | isJust $ telegramMessageAudio telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             AudioMessage'
                 (fromJust $ telegramMessageAudio telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --AudioMessage' <$> telegramMessageAudio telegramMessage
    | isJust $ telegramMessageDocument telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             DocumentMessage'
                 (fromJust $ telegramMessageDocument telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --DocumentMessage' <$> telegramMessageDocument telegramMessage
    | isJust $ telegramMessagePhoto telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             PhotoMessage'
                 (fromJust $ telegramMessagePhoto telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --PhotoMessage' <$> telegramMessagePhoto telegramMessage
    | isJust $ telegramMessageVideo telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             VideoMessage'
                 (fromJust $ telegramMessageVideo telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --VideoMessage' <$> telegramMessageVideo telegramMessage
    | isJust $ telegramMessageSticker telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             StickerMessage' $ fromJust $ telegramMessageSticker telegramMessage)
        --StickerMessage' <$> telegramMessageSticker telegramMessage
    | isJust $ telegramMessageVideoNote telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             VideoNoteMessage' $
             fromJust $ telegramMessageVideoNote telegramMessage)
        --VideoNoteMessage' <$> telegramMessageVideoNote telegramMessage
    | isJust $ telegramMessageVoice telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             VoiceMessage'
                 (fromJust $ telegramMessageVoice telegramMessage)
                 (telegramMessageCaption telegramMessage))
        --VoiceMessage' <$> telegramMessageVoice telegramMessage
    | isJust $ telegramMessageContact telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             ContactMessage' $ fromJust $ telegramMessageContact telegramMessage)
        --ContactMessage' <$> telegramMessageContact telegramMessage
    | isJust $ telegramMessageLocation telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             LocationMessage' $
             fromJust $ telegramMessageLocation telegramMessage)
        --LocationMessage' <$> telegramMessageLocation telegramMessage
    | isJust $ telegramMessageVenue telegramMessage =
        Just $
        L.UserMessage
            (telegramUserId user)
            (L.JustMessage $
             VenueMessage' $ fromJust $ telegramMessageVenue telegramMessage)
        --VenueMessage' <$> telegramMessageVenue telegramMessage
    | otherwise = Nothing
fromTgUpdateToUserMessage _ = Nothing
