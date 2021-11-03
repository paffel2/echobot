module Telegram.Responses where

import           Control.Monad  (MonadPlus (mzero))
import           Data.Aeson     (FromJSON (parseJSON), KeyValue ((.=)),
                                 Options (fieldLabelModifier), ToJSON (toJSON),
                                 Value (Object), camelTo2, defaultOptions,
                                 genericParseJSON, genericToJSON, object, (.:),
                                 (.:?))

import           GHC.Generics   (Generic)
import           Telegram.Types (Caption, UpdateId)
import           UsersLists     (ChatId, RepeatsNum)

data TelegramResponse a =
    TelegramResponse
        { telegramResponseOk          :: Bool
        , telegramResponseDescription :: Maybe String
        , telegramResponseResult      :: Maybe a
        }
    deriving (Show, Generic)

instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON (Object v) =
        TelegramResponse <$> v .: "ok" <*> v .:? "desciption" <*> v .:? "result"
    parseJSON _ = mzero

data TelegramUpdate =
    TelegramUpdate
        { telegramUpdateId            :: UpdateId
        , telegramUpdateMessage       :: Maybe TelegramMessage
        , telegramUpdateCallbackQuery :: Maybe TelegramCallbackQuery
        }
    deriving (Show, Generic)

instance FromJSON TelegramUpdate where
    parseJSON (Object v) =
        TelegramUpdate <$> v .: "update_id" <*> v .:? "message" <*>
        v .:? "callback_query"
    parseJSON _ = mzero

data TelegramUser =
    TelegramUser
        { telegramUserId                      :: ChatId
        , telegramUserIsBot                   :: Bool
        , telegramUserFirstName               :: String
        , telegramUserLastName                :: Maybe String
        , telegramUserUsername                :: Maybe String
        , telegramUserLanguageCode            :: Maybe String
        , telegramUserCanJoinGroups           :: Maybe Bool
        , telegramUserCanReadAllGroupMessages :: Maybe Bool
        , telegramUserSupportsInlineQueries   :: Maybe Bool
        }
    deriving (Show, Generic)

instance FromJSON TelegramUser where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 12}

data TelegramChat =
    TelegramChat
        { telegramChatId        :: ChatId
        , telegramChatType      :: String
        , telegramChatTitle     :: Maybe String
        , telegramChatUsername  :: Maybe String
        , telegramChatFirstName :: Maybe String
        , telegramChatLastName  :: Maybe String
        }
    deriving (Show, Generic)

instance FromJSON TelegramChat where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 12}

data TelegramMessage =
    TelegramMessage
        { telegramMessageMessageId            :: Int
        , telegramMessageFrom                 :: Maybe TelegramUser
        , telegramMessageSenderChat           :: Maybe TelegramChat
        , telegramMessageDate                 :: Int
        , telegramMessageChat                 :: TelegramChat
        , telegramMessageForwardFrom          :: Maybe TelegramUser
        , telegramMessageForwardFromChat      :: Maybe TelegramChat
        , telegramMessageForwardFromMessageId :: Maybe Int
        , telegramMessageForwardSignature     :: Maybe String
        , telegramMessageText                 :: Maybe String
        , telegramMessageEntities             :: Maybe [TelegramMessageEntity]
        , telegramMessageAnimation            :: Maybe TelegramAnimation
        , telegramMessageCaption              :: Maybe Caption
        , telegramMessageDocument             :: Maybe TelegramDocument
        , telegramMessageAudio                :: Maybe TelegramAudio
        , telegramMessagePhoto                :: Maybe [TelegramPhotoSize]
        , telegramMessageVideo                :: Maybe TelegramVideo
        , telegramMessageSticker              :: Maybe TelegramSticker
        , telegramMessageVideoNote            :: Maybe TelegramVideoNote
        , telegramMessageVoice                :: Maybe TelegramVoice
        , telegramMessageContact              :: Maybe TelegramContact
        , telegramMessageLocation             :: Maybe TelegramLocation
        , telegramMessageVenue                :: Maybe TelegramVenue
        }
    deriving (Show, Generic)

instance FromJSON TelegramMessage where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

{-data TelegramCommand
    = Help
    | Repeat
    deriving (Show) -}

type TelegramText = String

{-data TgMessage
    = TextMessage TelegramText
    | CommandMessage TelegramCommand
    | AnimationMessage TelegramAnimation
    | AudioMessage TelegramAudio
    | DocumentMessage TelegramDocument
    | PhotoMessage [TelegramPhotoSize]
    | VideoMessage TelegramVideo
    | StickerMessage TelegramSticker
    | VideoNoteMessage TelegramVideoNote
    | VoiceMessage TelegramVoice
    | ContactMessage TelegramContact
    | LocationMessage TelegramLocation
    | VenueMessage TelegramVenue
    deriving (Show) -}

data TgMessage
    = TextMessage TelegramText (Maybe [TelegramMessageEntity])
    | CommandMessage TelegramCommand
    | AnimationMessage' TelegramAnimation (Maybe Caption)
    | AudioMessage TelegramAudio (Maybe Caption)
    | DocumentMessage TelegramDocument (Maybe Caption)
    | PhotoMessage [TelegramPhotoSize] (Maybe Caption)
    | VideoMessage TelegramVideo (Maybe Caption)
    | StickerMessage TelegramSticker
    | VideoNoteMessage TelegramVideoNote
    | VoiceMessage TelegramVoice (Maybe Caption)
    | ContactMessage TelegramContact
    | LocationMessage TelegramLocation
    | VenueMessage TelegramVenue
    deriving (Show)

data TelegramCommand
    = Help
    | Repeat
    | Confirm
    deriving (Show)

data TelegramMessageEntity =
    TelegramMessageEntity
        { telegramMessageEntityType   :: String
        , telegramMessageEntityOffset :: Int
        , telegramMessageEntityLength :: Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramMessageEntity where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 21}

instance ToJSON TelegramMessageEntity where
    toJSON (TelegramMessageEntity ty off len) =
        object ["type" .= ty, "offset" .= off, "length" .= len]

data TelegramAnimation =
    TelegramAnimation
        { telegramAnimationFileId       :: String
        , telegramAnimationFileUniqueId :: String
        , telegramAnimationWidth        :: Integer
        , telegramAnimationHeight       :: Integer
        , telegramAnimationDuration     :: Integer
        , telegramAnimationThumb        :: Maybe TelegramPhotoSize
        , telegramAnimationFileName     :: Maybe String
        , telegramAnimationMimeType     :: Maybe String
        , telegramAnimationFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramAnimation where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 17}

data TelegramPhotoSize =
    TelegramPhotoSize
        { telegramPhotoSizeFileId       :: String
        , telegramPhotoSizeFileUniqueId :: String
        , telegramPhotoSizeWidth        :: Int
        , telegramPhotoSizeHeight       :: Int
        , telegramPhotoSizeFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramPhotoSize where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 17}

data TelegramDocument =
    TelegramDocument
        { telegramDocumentFileId       :: String
        , telegramDocumentFileUniqueId :: String
        , telegramDocumentThumb        :: Maybe TelegramPhotoSize
        , telegramDocumentFileName     :: Maybe String
        , telegramDocumentMimeType     :: Maybe String
        , telegramDocumentFileSize     :: Maybe Integer
        }
    deriving (Show, Generic)

instance FromJSON TelegramDocument where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 16}

data TelegramAudio =
    TelegramAudio
        { telegramAudioFileId       :: String
        , telegramAudioFileUniqueId :: String
        , telegramAudioDuration     :: Integer
        , telegramAudioPerformer    :: Maybe String
        , telegramAudioTitle        :: Maybe String
        , telegramAudioFileName     :: Maybe String
        , telegramAudioMimeType     :: Maybe String
        , telegramAudioFileSize     :: Maybe Int
        , telegramAudioThumb        :: Maybe TelegramPhotoSize
        }
    deriving (Show, Generic)

instance FromJSON TelegramAudio where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 13}

data TelegramVideo =
    TelegramVideo
        { telegramVideoFileId       :: String
        , telegramVideoFileUniqueId :: String
        , telegramVideoWidth        :: Integer
        , telegramVideoHeight       :: Integer
        , telegramVideoDuration     :: Integer
        , telegramVideoThumb        :: Maybe TelegramPhotoSize
        , telegramVideoFileName     :: Maybe String
        , telegramVideoMimeType     :: Maybe String
        , telegramVideoFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramVideo where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 13}

data TelegramSticker =
    TelegramSticker
        { telegramStickerFileId       :: String
        , telegramStickerFileUniqueId :: String
        , telegramStickerWidth        :: Int
        , telegramStickerHeight       :: Int
        , telegramStickerIsAnimated   :: Bool
        , telegramStickerThumb        :: Maybe TelegramPhotoSize
        , telegramStickerEmoji        :: Maybe String
        , telegramStickerSetName      :: Maybe String
        , telegramStickerMaskPosition :: Maybe TelegramMaskPosition
        , telegramStickerFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramSticker where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 15}

data TelegramMaskPosition =
    TelegramMaskPosition
        { telegramMaskPositionPoint  :: String
        , telegramMaskPositionXShift :: Double
        , telegramMaskPositionYShift :: Double
        , telegramMaskPositionScale  :: Double
        }
    deriving (Show, Generic)

instance FromJSON TelegramMaskPosition where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 20}

data TelegramVideoNote =
    TelegramVideoNote
        { telegramVideoNoteFileId       :: String
        , telegramVideoNoteFileUniqueId :: String
        , telegramVideoNoteLength       :: Integer
        , telegramVideoNoteDuration     :: Integer
        , telegramVideoNoteThumb        :: Maybe TelegramPhotoSize
        , telegramVideoNoteFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramVideoNote where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 17}

data TelegramVoice =
    TelegramVoice
        { telegramVoiceFileId       :: String
        , telegramVoiceFileUniqueId :: String
        , telegramVoiceDuration     :: Integer
        , telegramVoiceMimeType     :: Maybe String
        , telegramVoiceFileSize     :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramVoice where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 13}

data TelegramContact =
    TelegramContact
        { telegramContactPhoneNumber :: String
        , telegramContactFirstName   :: String
        , telegramContactLastName    :: Maybe String
        , telegramContactUserId      :: Maybe Int
        , telegramContactVcard       :: Maybe String
        }
    deriving (Show, Generic)

instance FromJSON TelegramContact where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}

data TelegramLocation =
    TelegramLocation
        { telegramLocationLatitude             :: Double
        , telegramLocationLongitude            :: Double
        , telegramLocationHorizontalAccuracy   :: Maybe Double
        , telegramLocationLivePeriod           :: Maybe Int
        , telegramLocationHeading              :: Maybe Int
        , telegramLocationProximityAlertRadius :: Maybe Int
        }
    deriving (Show, Generic)

instance FromJSON TelegramLocation where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 16}

data TelegramVenue =
    TelegramVenue
        { telegramVenueLocation        :: TelegramLocation
        , telegramVenueTitle           :: String
        , telegramVenueAddress         :: String
        , telegramVenueFoursquareId    :: Maybe String
        , telegramVenueFoursquareType  :: Maybe String
        , telegramVenueGooglePlaceId   :: Maybe String
        , telegramVenueGooglePlaceType :: Maybe String
        }
    deriving (Show, Generic)

instance FromJSON TelegramVenue where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 13}

data TelegramInlineKeyboardButton =
    TelegramInlineKeyboardButton
        { telegramInlineKeyboardButtonText         :: String
        , telegramInlineKeyboardButtonCallbackData :: String
        }
    deriving (Show, Generic)

instance ToJSON TelegramInlineKeyboardButton where
    toJSON (TelegramInlineKeyboardButton text callback) =
        object ["text" .= text, "callback_data" .= callback]

newtype TelegramInlineKeyboardMarkup =
    TelegramInlineKeyboardMarkup
        { telegramInlineKeyboardMarkupInlineKeyboard :: [[TelegramInlineKeyboardButton]]
        }
    deriving (Show, Generic)

instance ToJSON TelegramInlineKeyboardMarkup where
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 28}

data TelegramCallbackQuery =
    TelegramCallbackQuery
        { telegramCallbackQueryId           :: String
        , telegramCallbackQueryFrom         :: TelegramUser
        , telegramCallbackQueryMessage      :: Maybe TelegramMessage
        , telegramCallbackQueryChatInstance :: String
        , telegramCallbackQueryData         :: Maybe RepeatsNum
        }
    deriving (Show, Generic)

instance FromJSON TelegramCallbackQuery where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 21}
