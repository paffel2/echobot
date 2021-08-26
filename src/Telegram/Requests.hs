module Telegram.Requests where

import Data.Aeson
    ( KeyValue((.=))
    , Options(fieldLabelModifier, omitNothingFields)
    , ToJSON(toJSON)
    , camelTo2
    , defaultOptions
    , genericToJSON
    , object
    )
import GHC.Generics (Generic)
import Telegram.Responses (TelegramInlineKeyboardMarkup, TelegramMessageEntity)

data TelegramSendMessage =
    TelegramSendMessage
        { telegramSendMessageChatId :: Int
        , telegramSendMessageText :: String
        , telegramSendMessageEntities :: Maybe [TelegramMessageEntity]
        , telegramSendMessageReplyMarkup :: Maybe TelegramInlineKeyboardMarkup
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendMessage where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 19
                , omitNothingFields = True
                }

data TelegramSendAnimation =
    TelegramSendAnimation
        { telegramSendAnimationChatId :: Int
        , telegramSendAnimationAnimation :: String
        , telegramSendAnimationCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendAnimation where
    toJSON (TelegramSendAnimation ci anim (Just cap)) =
        object ["chat_id" .= ci, "animation" .= anim, "caption" .= cap]
    toJSON (TelegramSendAnimation ci anim _) =
        object ["chat_id" .= ci, "animation" .= anim]

data TelegramSendAudio =
    TelegramSendAudio
        { telegramSendAudioChatId :: Int
        , telegramSendAudioAudio :: String
        , telegramSendAudioCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendAudio where
    toJSON (TelegramSendAudio ci aud (Just cap)) =
        object ["chat_id" .= ci, "audio" .= aud, "caption" .= cap]
    toJSON (TelegramSendAudio ci aud _) =
        object ["chat_id" .= ci, "audio" .= aud]

data TelegramSendDocument =
    TelegramSendDocument
        { telegramSendDocumentChatId :: Int
        , telegramSendDocumentDocument :: String
        , telegramSendDocumentCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendDocument where
    toJSON (TelegramSendDocument ci doc (Just cap)) =
        object ["chat_id" .= ci, "document" .= doc, "caption" .= cap]
    toJSON (TelegramSendDocument ci doc _) =
        object ["chat_id" .= ci, "document" .= doc]

data TelegramSendPhoto =
    TelegramSendPhoto
        { telegramSendPhotoChatId :: Int
        , telegramSendPhotoPhoto :: String
        , telegramSendPhotoCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendPhoto where
    toJSON (TelegramSendPhoto ci ph (Just cap)) =
        object ["chat_id" .= ci, "photo" .= ph, "caption" .= cap]
    toJSON (TelegramSendPhoto ci ph _) = object ["chat_id" .= ci, "photo" .= ph]

data TelegramSendVideo =
    TelegramSendVideo
        { telegramSendVideoChatId :: Int
        , telegramSendVideoVideo :: String
        , telegramSendVideoCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendVideo where
    toJSON (TelegramSendVideo ci vid (Just cap)) =
        object ["chat_id" .= ci, "video" .= vid, "caption" .= cap]
    toJSON (TelegramSendVideo ci vid _) =
        object ["chat_id" .= ci, "video" .= vid]

data TelegramSendSticker =
    TelegramSendSticker
        { telegramSendStickerChatId :: Int
        , tlegramSendStickerSticker :: String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendSticker where
    toJSON (TelegramSendSticker ci stic) =
        object ["chat_id" .= ci, "sticker" .= stic]

data TelegramSendVideoNote =
    TelegramSendVideoNote
        { telegramSendVideoNoteChatId :: Int
        , tlegramSendVideoNoteVideoNote :: String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendVideoNote where
    toJSON (TelegramSendVideoNote ci note) =
        object ["chat_id" .= ci, "video_note" .= note]

data TelegramSendVoice =
    TelegramSendVoice
        { telegramSendVoiceChatId :: Int
        , telegramSendVoiceVoice :: String
        , telegramSendVoiceCaption :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendVoice where
    toJSON (TelegramSendVoice ci voi (Just cap)) =
        object ["chat_id" .= ci, "voice" .= voi, "caption" .= cap]
    toJSON (TelegramSendVoice ci voi _) =
        object ["chat_id" .= ci, "voice" .= voi]

data TelegramSendContact =
    TelegramSendContact
        { telegramSendContactChatId :: Int
        , telegramSendContactPhoneNumber :: String
        , telegramSendContactFirstName :: String
        , telegramSendContactLastName :: Maybe String
        , telegramSendContactVcard :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendContact where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 19
                , omitNothingFields = True
                }

data TelegramSendLocation =
    TelegramSendLocation
        { telegramSendLocationChatId :: Int
        , telegramSendLocationLatitude :: Double
        , telegramSendLocationLongitude :: Double
        , telegramSendLocationHorizontalAccuracy :: Maybe Double
        , telegramSendLocationLivePeriod :: Maybe Int
        , telegramSendLocationHeading :: Maybe Int
        , telegramSendLocationProximityAlertRadius :: Maybe Int
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendLocation where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 20
                , omitNothingFields = True
                }

data TelegramSendVenue =
    TelegramSendVenue
        { telegramSendVenueChatId :: Int
        , telegramSendVenueLatitude :: Double
        , telegramSendVenueLongitude :: Double
        , telegramSendVenueTitle :: String
        , telegramSendVenueAddress :: String
        , telegramSendVenueFoursquareId :: Maybe String
        , telegramSendVenueFousquareType :: Maybe String
        , telegramSendVenueGooglePlaceId :: Maybe String
        , telegramSendVenueGooglePlaceType :: Maybe String
        }
    deriving (Show, Generic)

instance ToJSON TelegramSendVenue where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 17
                , omitNothingFields = True
                }
