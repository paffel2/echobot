module TelegramResponses where

import Data.Aeson
import GHC.Generics
import Control.Monad
-------------------------------------------------------------------------------------------
data TelegramSendMessage = TelegramSendMessage { telegramSendMessageChatId :: Int
                                               , telegramSendMessageText :: String
                                               , telegramSendMessageEntities :: Maybe [TelegramMessageEntity]
                                               , telegramSendMessageReplyMarkup :: Maybe TelegramInlineKeyboardMarkup
                                               } deriving (Show, Generic)
{-instance ToJSON TelegramSendMessage where
    toJSON (TelegramSendMessage ci tt Nothing) = 
        object [ "chat_id" .= ci
               , "text" .= tt]

    toJSON (TelegramSendMessage ci tt (Just listEnt)) = 
        object [ "chat_id" .= ci
               , "text" .= tt
               , "entities".= listEnt]-}
instance ToJSON TelegramSendMessage where
    toJSON  = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19, omitNothingFields = True  }

data TelegramSendAnimation = TelegramSendAnimation { telegramSendAnimationChatId :: Int
                                                   , telegramSendAnimationAnimation :: String
                                                   , telegramSendAnimationCaption :: Maybe String
                                                   } deriving (Show, Generic)
instance ToJSON TelegramSendAnimation where
    toJSON (TelegramSendAnimation ci anim (Just cap)) =
        object [ "chat_id" .= ci
               , "animation" .= anim
               , "caption" .= cap] 

    toJSON (TelegramSendAnimation ci anim _ ) =
        object [ "chat_id" .= ci
               , "animation" .= anim
               ] 

data TelegramSendAudio = TelegramSendAudio { telegramSendAudioChatId :: Int
                                           , telegramSendAudioAudio :: String
                                           , telegramSendAudioCaption :: Maybe String 
                                           } deriving (Show, Generic)

instance ToJSON TelegramSendAudio where
    toJSON (TelegramSendAudio ci aud (Just cap)) =
        object [ "chat_id" .= ci
               , "audio" .= aud
               , "caption" .= cap] 

    toJSON (TelegramSendAudio ci aud _ ) =
        object [ "chat_id" .= ci
               , "audio" .= aud
               ]

data TelegramSendDocument = TelegramSendDocument { telegramSendDocumentChatId :: Int
                                                 , telegramSendDocumentDocument :: String
                                                 , telegramSendDocumentCaption :: Maybe String
                                                 } deriving (Show, Generic)

instance ToJSON TelegramSendDocument where
    toJSON (TelegramSendDocument ci doc (Just cap)) =
        object [ "chat_id" .= ci
               , "document" .= doc
               , "caption" .= cap] 

    toJSON (TelegramSendDocument ci doc _ ) =
        object [ "chat_id" .= ci
               , "document" .= doc
               ]

data TelegramSendPhoto = TelegramSendPhoto { telegramSendPhotoChatId :: Int
                                           , telegramSendPhotoPhoto :: String
                                           , telegramSendPhotoCaption :: Maybe String
                                           } deriving (Show, Generic)

instance ToJSON TelegramSendPhoto where
    toJSON (TelegramSendPhoto ci ph (Just cap)) =
        object [ "chat_id" .= ci
               , "photo" .= ph
               , "caption" .= cap]
    toJSON (TelegramSendPhoto ci ph _) =
        object [ "chat_id" .= ci
               , "photo" .= ph]


data TelegramSendVideo = TelegramSendVideo { telegramSendVideoChatId :: Int
                                           , telegramSendVideoVideo :: String
                                           , telegramSendVideoCaption :: Maybe String
                                           } deriving (Show, Generic)

instance ToJSON TelegramSendVideo where
    toJSON (TelegramSendVideo ci vid (Just cap)) =
        object [ "chat_id" .= ci
               , "video" .= vid
               , "caption" .= cap]
    toJSON (TelegramSendVideo ci vid _) =
        object [ "chat_id" .= ci
               , "video" .= vid]

data TelegramSendSticker = TelegramSendSticker { telegramSendStickerChatId :: Int
                                               , tlegramSendStickerSticker :: String
                                               } deriving (Show, Generic)

instance ToJSON TelegramSendSticker where
    toJSON (TelegramSendSticker ci stic) =
        object [ "chat_id" .= ci
               , "sticker" .= stic]

data TelegramSendVideoNote = TelegramSendVideoNote { telegramSendVideoNoteChatId :: Int
                                                   , tlegramSendVideoNoteVideoNote :: String
                                                   } deriving (Show, Generic)

instance ToJSON TelegramSendVideoNote where
    toJSON (TelegramSendVideoNote ci note) =
        object [ "chat_id" .= ci
               , "video_note" .= note]

data TelegramSendVoice = TelegramSendVoice { telegramSendVoiceChatId :: Int
                                           , telegramSendVoiceVoice :: String
                                           , telegramSendVoiceCaption :: Maybe String 
                                           } deriving (Show, Generic)

instance ToJSON TelegramSendVoice where
    toJSON (TelegramSendVoice ci voi (Just cap)) =
        object [ "chat_id" .= ci
               , "voice" .= voi
               , "caption" .= cap] 

    toJSON (TelegramSendVoice ci voi _ ) =
        object [ "chat_id" .= ci
               , "voice" .= voi]

--переписать все как здесь
data TelegramSendContact = TelegramSendContact { telegramSendContactChatId :: Int
                                               , telegramSendContactPhoneNumber :: String
                                               , telegramSendContactFirstName :: String
                                               , telegramSendContactLastName :: Maybe String
                                               , telegramSendContactVcard :: Maybe String
                                               } deriving (Show, Generic)
instance ToJSON TelegramSendContact where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19, omitNothingFields = True  }


data TelegramSendLocation = TelegramSendLocation { telegramSendLocationChatId :: Int
                                                 , telegramSendLocationLatitude :: Double
                                                 , telegramSendLocationLongitude :: Double
                                                 , telegramSendLocationHorizontalAccuracy :: Maybe Double
                                                 , telegramSendLocationLivePeriod :: Maybe Int
                                                 , telegramSendLocationHeading :: Maybe Int
                                                 , telegramSendLocationProximityAlertRadius :: Maybe Int 
                                                 } deriving (Show, Generic)
instance ToJSON TelegramSendLocation where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20, omitNothingFields = True  }


data TelegramSendVenue = TelegramSendVenue { telegramSendVenueChatId :: Int
                                           , telegramSendVenueLatitude :: Double
                                           , telegramSendVenueLongitude :: Double
                                           , telegramSendVenueTitle :: String
                                           , telegramSendVenueAddress :: String
                                           , telegramSendVenueFoursquareId :: Maybe String
                                           , telegramSendVenueFousquareType :: Maybe String
                                           , telegramSendVenueGooglePlaceId :: Maybe String
                                           , telegramSendVenueGooglePlaceType :: Maybe String
                                           } deriving (Show, Generic)
instance ToJSON TelegramSendVenue where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17, omitNothingFields = True  }



-------------------------------------------------------------------------------------------

data TelegramResponse a = TelegramResponse  { telegramResponseOk :: Bool
                                            , telegramResponseDescription :: Maybe String
                                            , telegramResponseResult :: Maybe a
                                            } deriving (Show, Generic)
instance FromJSON a => FromJSON (TelegramResponse a) where
    parseJSON (Object v) = 
        TelegramResponse <$> v .: "ok"
                         <*> v .:? "desciption"
                         <*> v .:? "result"
    parseJSON _ = mzero

data TelegramUpdate = TelegramUpdate { telegramUpdateId :: Int
                                     , telegramUpdateMessage :: Maybe TelegramMessage
                                   --  , telegramUpdateEditedMessage :: Maybe TelegramMessage
                                    -- , telegramUpdateChannelPost :: Maybe TelegramMessage
                                    -- , telegramUpdateEditedChannelPost :: Maybe TelegramMessage
                                     , telegramUpdateCallbackQuery :: Maybe TelegramCallbackQuery
                                     } deriving (Show, Generic)
instance FromJSON TelegramUpdate where
    parseJSON (Object v) =
        TelegramUpdate <$> v .: "update_id"
                       <*> v .:? "message"
                     --  <*> v .:? "edited_message"
                     --  <*> v .:? "channel_post"
                     --  <*> v .:? "edited_channel_post"
                       <*> v .:? "callback_query"
    parseJSON _ = mzero


data TelegramUser = TelegramUser { telegramUserId :: Int
                                 , telegramUserIsBot :: Bool
                                 , telegramUserFirstName :: String
                                 , telegramUserLastName :: Maybe String
                                 , telegramUserUsername :: Maybe String
                                 , telegramUserLanguageCode :: Maybe String
                                 , telegramUserCanJoinGroups :: Maybe Bool
                                 , telegramUserCanReadAllGroupMessages :: Maybe Bool
                                 , telegramUserSupportsInlineQueries :: Maybe Bool
                                 } deriving (Show, Generic)
instance FromJSON TelegramUser where
    parseJSON (Object v) = 
        TelegramUser <$> v .: "id"
                     <*> v .: "is_bot"
                     <*> v .: "first_name"
                     <*> v .:? "last_name"
                     <*> v .:? "username"
                     <*> v .:? "language_code"
                     <*> v .:? "can_join_groups"
                     <*> v .:? "can_read_all_group_messages"
                     <*> v .:? "supports_inline_queries"
    parseJSON _ = mzero


data TelegramChat = TelegramChat { telegramChatId :: Int
                                 , telegramChatType :: String
                                 , telegramChatTitle :: Maybe String
                                 , telegramChatUsername :: Maybe String
                                 , telegramChatFirstName :: Maybe String
                                 , telegramChatLastName :: Maybe String
                                 } deriving (Show, Generic)
instance FromJSON TelegramChat where
    parseJSON (Object v) =
        TelegramChat <$> v .: "id" 
                     <*> v .: "type"
                     <*> v .:? "title"
                     <*> v .:? "username"
                     <*> v .:? "first_name"
                     <*> v .:? "last_name"
    parseJSON _ = mzero
--дополнить
data TelegramMessage = TelegramMessage { telegramMessageMessageId :: Int 
                                       , telegramMessageFrom :: Maybe TelegramUser
                                       , telegramMessageSenderChat :: Maybe TelegramChat
                                       , telegramMessageDate :: Int
                                       , telegramMessageChat :: TelegramChat
                                       , telegramMessageForwardFrom :: Maybe TelegramUser
                                       , telegramMessageForwardFromChat :: Maybe TelegramChat
                                       , telegramMessageForwardFromMessageId :: Maybe Int
                                       , telegramMessageForwardSignature :: Maybe String
                                       , telegramMessageText :: Maybe String
                                       , telegramMessageEntities :: Maybe [TelegramMessageEntity]
                                       , telegramMessageAnimation :: Maybe TelegramAnimation
                                       , telegramMessageCaption :: Maybe String
                                       , telegramMessageDocument :: Maybe TelegramDocument
                                       , telegramMessageAudio :: Maybe TelegramAudio
                                       , telegramMessagePhoto :: Maybe [TelegramPhotoSize]
                                       , telegramMessageVideo :: Maybe TelegramVideo
                                       , telegramMessageSticker :: Maybe TelegramSticker
                                       , telegramMessageVideoNote :: Maybe TelegramVideoNote
                                       , telegramMessageVoice :: Maybe TelegramVoice
                                       , telegramMessageContact :: Maybe TelegramContact
                                       , telegramMessageLocation :: Maybe TelegramLocation
                                       , telegramMessageVenue :: Maybe TelegramVenue
                                     --  , telegramMessageReplyKeyboardMarkup :: Maybe TelegramReplyKeyboardMarkup
                                       } deriving (Show, Generic)
instance FromJSON TelegramMessage where
    parseJSON (Object v) =
        TelegramMessage <$> v .: "message_id"
                        <*> v .:? "from"
                        <*> v .:? "sender_chat"
                        <*> v .: "date"
                        <*> v .: "chat"
                        <*> v .:? "forward_from"
                        <*> v .:? "forward_from_chat"
                        <*> v .:? "forward_from_message_id"
                        <*> v .:? "forward_from_signature"
                        <*> v .:? "text"
                        <*> v .:? "entities"
                        <*> v .:? "animation"
                        <*> v .:? "caption"
                        <*> v .:? "document"
                        <*> v .:? "audio"
                        <*> v .:? "photo"
                        <*> v .:? "video"
                        <*> v .:? "sticker"
                        <*> v .:? "video_note"
                        <*> v .:? "voice"
                        <*> v .:? "contact"
                        <*> v .:? "location"
                        <*> v .:? "venue"
                      --  <*> v .:? "reply_markup"
    parseJSON _ = mzero

data TelegramMessageEntity = TelegramMessageEntity { telegramMessageEntityType :: String
                                                   , telegramMessageEntityOffset :: Int
                                                   , telegramMessageEntityLength :: Int
                                                   } deriving (Show, Generic)
instance FromJSON TelegramMessageEntity where
    parseJSON (Object v) = 
        TelegramMessageEntity <$> v .: "type"
                              <*> v .: "offset"
                              <*> v .: "length" 

instance ToJSON TelegramMessageEntity where
    toJSON (TelegramMessageEntity ty off len) =
        object [ "type" .= ty
               , "offset" .= off
               , "length" .= len] 


data TelegramAnimation = TelegramAnimation { telegramAnimationFileId :: String
                                           , telegramAnimationFileUniqueId :: String
                                           , telegramAnimationWidth :: Integer
                                           , telegramAnimationHeight :: Integer
                                           , telegramAnimationDuration :: Integer
                                           , telegramAnimationThumb :: Maybe TelegramPhotoSize
                                           , telegramAnimationFileName :: Maybe String
                                           , telegramAnimationMimeType :: Maybe String
                                           , telegramAnimationFileSize :: Maybe Int
                                           } deriving (Show, Generic)
instance FromJSON TelegramAnimation where
    parseJSON (Object v) = 
        TelegramAnimation <$> v .: "file_id"
                          <*> v .: "file_unique_id"
                          <*> v .: "width"
                          <*> v .: "height"
                          <*> v .: "duration"
                          <*> v .:? "thumb"
                          <*> v .:? "file_name"
                          <*> v .:? "mime_type"
                          <*> v .:? "file_size"

data TelegramPhotoSize = TelegramPhotoSize { telegramPhotoSizeFileId :: String
                                           , telegramPhotoSizeFileUniqueId :: String
                                           , telegramPhotoSizeWidth :: Int
                                           , telegramPhotoSizeHeight :: Int
                                           , telegramPhotoSizeFileSize :: Maybe Int 
                                           } deriving (Show, Generic)
instance FromJSON TelegramPhotoSize where
    parseJSON (Object v) =
        TelegramPhotoSize <$> v .: "file_id"
                          <*> v .: "file_unique_id"
                          <*> v .: "width"
                          <*> v .: "height"
                          <*> v .:? "file_size"  

data TelegramDocument = TelegramDocument { telegramDocumentFileId :: String
                                         , telegramDocumentFileUniqueId :: String
                                         , telegramDocumentThumb :: Maybe TelegramPhotoSize
                                         , telegramDocumentFileName :: Maybe String
                                         , telegramDocumentMimeType :: Maybe String
                                         , telegramDocumentFileSize :: Maybe Integer
                                         } deriving (Show, Generic)
instance FromJSON TelegramDocument where
    parseJSON (Object v) = 
        TelegramDocument <$> v .: "file_id"
                         <*> v .: "file_unique_id"
                         <*> v .:? "thumb"
                         <*> v .:? "file_name"
                         <*> v .:? "mime_type"
                         <*> v .:? "file_size"


data TelegramAudio = TelegramAudio { telegramAudioFileId :: String
                                   , telegramAudioFileUniqueId :: String
                                   , telegramAudioDuration :: Integer
                                   , telegramAudioPerformer :: Maybe String
                                   , telegramAudioTitle :: Maybe String
                                   , telegramAudioFileName :: Maybe String
                                   , telegramAudioMimeType :: Maybe String
                                   , telegramAudioFileSize :: Maybe Int
                                   , telegramAudioThumb :: Maybe TelegramPhotoSize
                                   } deriving (Show, Generic)
instance FromJSON TelegramAudio where
    parseJSON (Object v) = 
        TelegramAudio <$> v .: "file_id"
                      <*> v .: "file_unique_id"
                      <*> v .: "duration"
                      <*> v .:? "performer"
                      <*> v .:? "title"
                      <*> v .:? "file_name"
                      <*> v .:? "mime_type"
                      <*> v .:? "file_size"
                      <*> v .:? "thumb"


data TelegramVideo = TelegramVideo { telegramVideoFileId :: String
                                   , telegramVideoFileUniqueId :: String
                                   , telegramVideoWidth :: Integer
                                   , telegramVideoHeight :: Integer
                                   , telegramVideoDuration :: Integer
                                   , telegramVideoThumb :: Maybe TelegramPhotoSize
                                   , telegramVideoFileName :: Maybe String
                                   , telegramVideoMimeType :: Maybe String
                                   , telegramVideoFileSize :: Maybe Int
                                   } deriving (Show, Generic)
instance FromJSON TelegramVideo where
    parseJSON (Object v) = 
        TelegramVideo <$> v .: "file_id"
                      <*> v .: "file_unique_id"
                      <*> v .: "width"
                      <*> v .: "height"
                      <*> v .: "duration"
                      <*> v .:? "thumb"
                      <*> v .:? "file_name"
                      <*> v .:? "mime_type"
                      <*> v .:? "file_size"


data TelegramSticker = TelegramSticker { telegramStickerFileId :: String
                                       , telegramStickerFileUniqueId :: String
                                       , telegramStickerWidth :: Int
                                       , telegramStickerHeight :: Int
                                       , telegramStickerIsAnimated :: Bool
                                       , telegramStickerThumb :: Maybe TelegramPhotoSize
                                       , telegramStickerEmoji :: Maybe String
                                       , telegramStickerSetName :: Maybe String
                                       , telegramStickerMaskPosition :: Maybe TelegramMaskPosition
                                       , telegramStickerFileSize :: Maybe Int 
                                       } deriving (Show, Generic)
instance FromJSON TelegramSticker where
    parseJSON (Object v) = 
        TelegramSticker <$> v .: "file_id"
                        <*> v .: "file_unique_id"
                        <*> v .: "width"
                        <*> v .: "height"
                        <*> v .: "is_animated"
                        <*> v .:? "thumb"
                        <*> v .:? "emoji"
                        <*> v .:? "set_name"
                        <*> v .:? "mask_position"
                        <*> v .:? "file_size"

data TelegramMaskPosition = TelegramMaskPosition { telegramMaskPositionPoint :: String
                                                 , telegramMaskPositionXShift :: Double
                                                 , telegramMaskPositionYShift :: Double
                                                 , telegramMaskPositionScale :: Double
                                                 } deriving (Show, Generic)

instance FromJSON TelegramMaskPosition where
    parseJSON (Object v) = 
        TelegramMaskPosition <$> v .: "point"
                             <*> v .: "x_shift"
                             <*> v .: "y_shift"
                             <*> v .: "scale"


data TelegramVideoNote = TelegramVideoNote { telegramVideoNoteFileId :: String
                                           , telegramVideoNoteFileUniqueId :: String
                                           , telegramVideoNoteLength :: Integer
                                           , telegramVideoNoteDuration :: Integer
                                           , telegramVideoNoteThumb :: Maybe TelegramPhotoSize
                                           , telegramVideoNoteFileSize :: Maybe Int
                                           } deriving (Show, Generic)
instance FromJSON TelegramVideoNote where
    parseJSON (Object v) = 
        TelegramVideoNote <$> v .: "file_id"
                          <*> v .: "file_unique_id"
                          <*> v .: "length"
                          <*> v .: "duration"
                          <*> v .:? "thumb"
                          <*> v .:? "file_size"

data TelegramVoice = TelegramVoice { telegramVoiceFileId :: String
                                   , telegramVoiceFileUniqueId :: String
                                   , telegramVoiceDuration :: Integer
                                   , telegramVoiceMimeType :: Maybe String
                                   , telegramVoiceFileSize :: Maybe Int
                                   } deriving (Show, Generic)
instance FromJSON TelegramVoice where
    parseJSON (Object v) = 
        TelegramVoice <$> v .: "file_id"
                      <*> v .: "file_unique_id"
                      <*> v .: "duration"
                      <*> v .:? "mime_type"
                      <*> v .:? "file_size"

data TelegramContact = TelegramContact { telegramContactPhoneNumber :: String
                                       , telegramContactFirstName :: String
                                       , telegramContactLastName :: Maybe String
                                       , telegramContactUserId :: Maybe Int
                                       , telegramContactVcard :: Maybe String
                                       } deriving (Show, Generic)
instance FromJSON TelegramContact where
    parseJSON (Object v) = 
          TelegramContact <$> v .: "phone_number"
                          <*> v .: "first_name"
                          <*> v .:? "last_name"
                          <*> v .:? "user_id"
                          <*> v .:? "vcard"

data TelegramLocation = TelegramLocation { telegramLocationLatitude :: Double
                                         , telegramLocationLongitude :: Double
                                         , telegramLocationHorizontalAccuracy :: Maybe Double
                                         , telegramLocationLivePeriod :: Maybe Int
                                         , telegramLocationHeading :: Maybe Int
                                         , telegramLocationProximityAlertRadius :: Maybe Int
                                         } deriving (Show, Generic)
instance FromJSON TelegramLocation where
    parseJSON  = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16}




data TelegramVenue = TelegramVenue { telegramVenueLocation :: TelegramLocation
                                   , telegramVenueTitle :: String
                                   , telegramVenueAddress :: String
                                   , telegramVenueFoursquareId :: Maybe String
                                   , telegramVenueFoursquareType :: Maybe String
                                   , telegramVenueGooglePlaceId :: Maybe String
                                   , telegramVenueGooglePlaceType :: Maybe String
                                   } deriving (Show, Generic)
instance FromJSON TelegramVenue where
    parseJSON  = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13}


data TelegramInlineKeyboardButton = TelegramInlineKeyboardButton { telegramInlineKeyboardButtonText :: String
                                                                 , telegramInlineKeyboardButtonCallbackData :: String } deriving (Show, Generic)
instance ToJSON TelegramInlineKeyboardButton where
    toJSON (TelegramInlineKeyboardButton text callback) = object [ "text" .= text
                                                                 , "callback_data" .= callback]

data TelegramInlineKeyboardMarkup = TelegramInlineKeyboardMarkup { telegramInlineKeyboardMarkupInlineKeyboard :: [[TelegramInlineKeyboardButton]]} deriving (Show, Generic)
instance ToJSON TelegramInlineKeyboardMarkup where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 28}




data TelegramCallbackQuery = TelegramCallbackQuery { telegramCallbackQueryId :: String
                                                   , telegramCallbackQueryFrom :: TelegramUser
                                                   , telegramCallbackQueryMessage :: Maybe TelegramMessage
                                                   , telegramCallbackQueryChatInstance :: String
                                                   , telegramCallbackQueryData :: Maybe String
                                                   } deriving (Show, Generic)
instance FromJSON TelegramCallbackQuery where
    parseJSON (Object v) =
        TelegramCallbackQuery <$> v .: "id"
                              <*> v .: "from"
                              <*> v .: "message"
                              <*> v .: "chat_instance"
                              <*> v .:? "data"

--------------------------------------------------------------------------------------------------------
data TelegramUserBaseUser = TelegramUserBaseUser { telegramUserBaseUserId :: Int
                                                 , telegramUserBaseUserRepeat :: Int
                                                 } deriving (Show, Generic)

instance FromJSON TelegramUserBaseUser where
     parseJSON (Object v) = 
        TelegramUserBaseUser <$> v .: "id"
                             <*> v .: "repeat"

data TelegramUserBase = TelegramUserBase { telegramUserBaseUsers :: [ TelegramUserBaseUser]
                                         } deriving (Show, Generic)
instance FromJSON TelegramUserBase where
     parseJSON (Object v) = 
        TelegramUserBase <$> v .: "users"








{-data TelegramReplyKeyboardRemove = TelegramReplyKeyboardRemove { telegramReplyKeyboardRemoveRemoveKeyboard :: Bool} deriving (Show, Generic)

instance FromJSON TelegramReplyKeyboardRemove where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27}
instance ToJSON TelegramReplyKeyboardRemove where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27, omitNothingFields = True  }

data TelegramKeyboardButton = TelegramKeyboardButton { telegramKeyboardButtonText :: String } deriving (Show, Generic)
instance FromJSON TelegramKeyboardButton where
    parseJSON (Object v) = 
          TelegramKeyboardButton <$> v .: "text"

instance ToJSON TelegramKeyboardButton where
    toJSON (TelegramKeyboardButton text) = object ["text" .= text]

data TelegramReplyKeyboardMarkup = TelegramReplyKeyboardMarkup { telegramReplyKeyboardMarkupKeyboard :: [[TelegramKeyboardButton]]
                                                               , telegramReplyKeyboardMarkupResizeKeyboard :: Maybe Bool
                                                               } deriving (Show, Generic)
instance FromJSON TelegramReplyKeyboardMarkup where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27}
instance ToJSON TelegramReplyKeyboardMarkup where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 27, omitNothingFields = True  }-}