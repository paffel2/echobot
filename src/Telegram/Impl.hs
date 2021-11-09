{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Impl where

import           Data.Maybe         (fromJust, isJust)
import qualified Echo               as E
import           Telegram.Responses (TelegramCallbackQuery (TelegramCallbackQuery, telegramCallbackQueryData, telegramCallbackQueryFrom, telegramCallbackQueryMessage),
                                     TelegramMessage (TelegramMessage, telegramMessageAnimation, telegramMessageAudio, telegramMessageCaption, telegramMessageContact, telegramMessageDocument, telegramMessageEntities, telegramMessageFrom, telegramMessageLocation, telegramMessagePhoto, telegramMessageSticker, telegramMessageText, telegramMessageVenue, telegramMessageVideo, telegramMessageVideoNote, telegramMessageVoice),
                                     TelegramUpdate (TelegramUpdate, telegramUpdateCallbackQuery, telegramUpdateMessage),
                                     TelegramUser (telegramUserId),
                                     TgMessage (AnimationMessage, AudioMessage, ContactMessage, DocumentMessage, LocationMessage, PhotoMessage, StickerMessage, TextMessage, VenueMessage, VideoMessage, VideoNoteMessage, VoiceMessage))

fromTgUpdateToUserMessage :: TelegramUpdate -> Maybe (E.UserMessage TgMessage)
fromTgUpdateToUserMessage TelegramUpdate {telegramUpdateCallbackQuery = (Just TelegramCallbackQuery { telegramCallbackQueryData = Just n
                                                                                                    , telegramCallbackQueryMessage = Just _
                                                                                                    , telegramCallbackQueryFrom = user
                                                                                                    })} =
    Just $ E.UserMessage (telegramUserId user) (E.CommandMessage $ E.Repeat n)
fromTgUpdateToUserMessage TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                         , telegramUpdateMessage = (Just telegramMessage@TelegramMessage {telegramMessageFrom = Just user})
                                         }
    | telegramMessageText telegramMessage == Just "/repeat" =
        Just $
        E.UserMessage (telegramUserId user) (E.CommandMessage E.ChoicesRequest)
    | telegramMessageText telegramMessage == Just "/help" =
        Just $ E.UserMessage (telegramUserId user) (E.CommandMessage E.Help)
    | isJust $ telegramMessageText telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             TextMessage
                 (fromJust $ telegramMessageText telegramMessage)
                 (telegramMessageEntities telegramMessage))
    | isJust $ telegramMessageAnimation telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             AnimationMessage
                 (fromJust $ telegramMessageAnimation telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessageAudio telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             AudioMessage
                 (fromJust $ telegramMessageAudio telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessageDocument telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             DocumentMessage
                 (fromJust $ telegramMessageDocument telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessagePhoto telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             PhotoMessage
                 (fromJust $ telegramMessagePhoto telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessageVideo telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             VideoMessage
                 (fromJust $ telegramMessageVideo telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessageSticker telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             StickerMessage $ fromJust $ telegramMessageSticker telegramMessage)
    | isJust $ telegramMessageVideoNote telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             VideoNoteMessage $
             fromJust $ telegramMessageVideoNote telegramMessage)
    | isJust $ telegramMessageVoice telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             VoiceMessage
                 (fromJust $ telegramMessageVoice telegramMessage)
                 (telegramMessageCaption telegramMessage))
    | isJust $ telegramMessageContact telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             ContactMessage $ fromJust $ telegramMessageContact telegramMessage)
    | isJust $ telegramMessageLocation telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             LocationMessage $
             fromJust $ telegramMessageLocation telegramMessage)
    | isJust $ telegramMessageVenue telegramMessage =
        Just $
        E.UserMessage
            (telegramUserId user)
            (E.JustMessage $
             VenueMessage $ fromJust $ telegramMessageVenue telegramMessage)
    | otherwise = Nothing
fromTgUpdateToUserMessage _ = Nothing
{-fromTgUpdateToUserMessage' :: TelegramUpdate -> Maybe (E.UserMessage TgMessage)
fromTgUpdateToUserMessage' TelegramUpdate {telegramUpdateCallbackQuery = (Just TelegramCallbackQuery { telegramCallbackQueryData = Just n
                                                                                                     , telegramCallbackQueryMessage = Just _
                                                                                                     , telegramCallbackQueryFrom = user
                                                                                                     })} =
    Just $ E.UserMessage (telegramUserId user) (E.CommandMessage $ E.Repeat n)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageText = Just "/repeat"
                                                                                                          })
                                          } =
    Just $
    E.UserMessage (telegramUserId user) (E.CommandMessage E.ChoicesRequest)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageText = Just "/help"
                                                                                                          })
                                          } =
    Just $ E.UserMessage (telegramUserId user) (E.CommandMessage E.Help)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageText = Just someText
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         TextMessage someText (telegramMessageEntities telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageAnimation = Just animation
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         AnimationMessage animation (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageAudio = Just audio
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         AudioMessage audio (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageDocument = Just document
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         DocumentMessage document (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessagePhoto = Just photo
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         PhotoMessage photo (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageVideo = Just video
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         VideoMessage video (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageSticker = Just sticker
                                                                                                          })
                                          } =
    Just $
    E.UserMessage (telegramUserId user) (E.JustMessage $ StickerMessage sticker)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageVideoNote = Just videoNote
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $ VideoNoteMessage videoNote)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageVoice = Just voice
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $
         VoiceMessage voice (telegramMessageCaption telegramMessage))
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageContact = Just contact
                                                                                                          })
                                          } =
    Just $
    E.UserMessage (telegramUserId user) (E.JustMessage $ ContactMessage contact)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageLocation = Just location
                                                                                                          })
                                          } =
    Just $
    E.UserMessage
        (telegramUserId user)
        (E.JustMessage $ LocationMessage location)
fromTgUpdateToUserMessage' TelegramUpdate { telegramUpdateCallbackQuery = Nothing
                                          , telegramUpdateMessage = (Just telegramMessage@TelegramMessage { telegramMessageFrom = Just user
                                                                                                          , telegramMessageVenue = Just venue
                                                                                                          })
                                          } =
    Just $
    E.UserMessage (telegramUserId user) (E.JustMessage $ VenueMessage venue)
fromTgUpdateToUserMessage' _ = Nothing -}
