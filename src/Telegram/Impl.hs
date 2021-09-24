module Telegram.Impl where

import Data.Maybe (isJust)
import Telegram.Responses
    ( TelegramCommand(Help, Repeat)
    , TelegramMessage(telegramMessageAnimation, telegramMessageAudio,
                telegramMessageContact, telegramMessageDocument,
                telegramMessageLocation, telegramMessagePhoto,
                telegramMessageSticker, telegramMessageText, telegramMessageVenue,
                telegramMessageVideo, telegramMessageVideoNote,
                telegramMessageVoice)
    , TgMessage(..)
    )

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
