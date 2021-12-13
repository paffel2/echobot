module Vk.Responses where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON),
                                      Options (fieldLabelModifier),
                                      Value (Object), camelTo2, defaultOptions,
                                      genericParseJSON, (.:))
import           GHC.Generics        (Generic)
import           UsersLists          (ChatId, RepeatsNum)
import           Vk.Types            (Pts, Ts)

newtype VkResponse =
    VkResponse
        { someResponse :: VkResponseType
        }
    deriving (Show, Generic)

instance FromJSON VkResponse where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 4}

data VkResponseType =
    Server
        { serverServer   :: Maybe String
        , serverKey      :: Maybe String
        , serverTS       :: Maybe Ts
        , serverPTS      :: Maybe Pts
        , serverNewPTS   :: Maybe Int
        , serverMessages :: Maybe VkMessages
        }
    deriving (Show, Generic)

instance FromJSON VkResponseType where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}

data VkMessages =
    VkMessages
        { vkMessagesCount :: Int
        , vkMessagesItems :: [VkItem]
        }
    deriving (Show, Generic)

instance FromJSON VkMessages where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 10}

data VkItem =
    VkItem
        { vkItemId          :: Maybe Int
        , vkItemFromId      :: ChatId
        , vkItemText        :: String
        , vkItemAttachments :: [VkAttachment]
        , vkItemImportant   :: Maybe Bool
        , vkItemGeo         :: Maybe VkGeo
        , vkItemFwdMessages :: Maybe [VkItem]
        , vkItemPayload     :: Maybe RepeatsNum
        }
    deriving (Show, Generic)

instance FromJSON VkItem where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}

data VkAttachment
    = VkAttachmentPhoto VkPhoto
    | VkAttachmentDoc VkDoc
    | VkAttachmentVideo VkVideo
    | VkAttachmentAudio VkAudio
    | VkAttachmentWall VkWall
    | VkAttachmentMarket VkMarket
    | VkAttachmentStory VkStory
    | VkAttachmentPoll VkPoll
    | VkAttachmentSticker VkSticker
    | VkAttachmentAudioMessage VkAudioMessage
    deriving (Show, Generic)

instance FromJSON VkAttachment where
    parseJSON (Object v) =
        (VkAttachmentPhoto <$> v .: "photo") <|>
        (VkAttachmentDoc <$> v .: "doc") <|>
        (VkAttachmentVideo <$> v .: "video") <|>
        (VkAttachmentAudio <$> v .: "audio") <|>
        (VkAttachmentWall <$> v .: "wall") <|>
        (VkAttachmentMarket <$> v .: "market") <|>
        (VkAttachmentStory <$> v .: "story") <|>
        (VkAttachmentPoll <$> v .: "poll") <|>
        (VkAttachmentSticker <$> v .: "sticker") <|>
        (VkAttachmentAudioMessage <$> v .: "audio_message")
    parseJSON _ = mzero

data VkCommand
    = Help
    | Repeat
    | Confirm

data VkMessageTypes
    = VkTextMessage String
    | VkGeoMessage VkGeo (Maybe String)
    | VkWithAttachmentsMessage [VkAttachment] (Maybe String)

data VkPhoto =
    VkPhoto
        { vkPhotoId        :: Int
        , vkPhotoOwnerId   :: Int
        , vkPhotoAccessKey :: String
        , vkPhotoText      :: String
        }
    deriving (Show, Generic)

instance FromJSON VkPhoto where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}

data VkDoc =
    VkDoc
        { vkDocId        :: Int
        , vkDocOwnerId   :: Int
        , vkDocAccessKey :: String
        }
    deriving (Show, Generic)

instance FromJSON VkDoc where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 5}

data VkVideo =
    VkVideo
        { vkVideoId        :: Int
        , vkVideoOwnerId   :: Int
        , vkVideoAccessKey :: String
        }
    deriving (Show, Generic)

instance FromJSON VkVideo where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}

data VkAudio =
    VkAudio
        { vkAudioId      :: Int
        , vkAudioOwnerId :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkAudio where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}

data VkWall =
    VkWall
        { vkWallFromId :: Int
        , vkWallId     :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkWall where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}

data VkMarket =
    VkMarket
        { vkMarketId      :: Int
        , vkMarketOwnerId :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkMarket where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 8}

data VkStory =
    VkStory
        { vkStoryId      :: Int
        , vkStoryOwnerId :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkStory where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}

data VkPoll =
    VkPoll
        { vkPollId      :: Int
        , vkPollOwnerId :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkPoll where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 6}

data VkSticker =
    VkSticker
        { vkStickerProductId :: Int
        , vkStickerStickerId :: Int
        }
    deriving (Show, Generic)

instance FromJSON VkSticker where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 9}

data VkGeo =
    VkGeo
        { vkGeoType        :: String
        , vkGeoCoordinates :: VkCoordinates
        , vkGeoPlace       :: VkPlace
        }
    deriving (Show, Generic)

instance FromJSON VkGeo where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 5}

data VkCoordinates =
    VkCoordinates
        { vkCoordinatesLatitude  :: Double
        , vkCoordinatesLongitude :: Double
        }
    deriving (Show, Generic)

instance FromJSON VkCoordinates where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 13}

data VkPlace =
    VkPlace
        { vkPlaceCountry :: String
        , vkPlaceCity    :: String
        , vkPlaceTitle   :: String
        }
    deriving (Show, Generic)

instance FromJSON VkPlace where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 7}

data VkAudioMessage =
    VkAudioMessage
        { vkAudioMessageId        :: Int
        , vkAudioMessageOwnerId   :: Int
        , vkAudioMessageAccessKey :: String
        }
    deriving (Show, Generic)

instance FromJSON VkAudioMessage where
    parseJSON =
        genericParseJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 14}
