module VkResponses where
import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Applicative

data VkResponse = VkResponse { someResponse :: VkResponseType
                            } deriving (Show, Generic)
instance FromJSON VkResponse where
    parseJSON (Object v) = 
        VkResponse <$> v .: "response"

data VkResponseType = Server {   serverServer :: Maybe String
                               , serverKey :: Maybe String
                               , serverTS :: Maybe Int
                               , serverPTS :: Maybe Int
                               , serverNewPTS :: Maybe Int
                               , serverMessages :: Maybe VkMessages
                               } deriving (Show, Generic)
instance FromJSON VkResponseType where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6}
    {-parseJSON (Object v) = 
        Server <$> v .: "server"
               <*> v .: "key"
               <*> v .: "ts"
               <*> v .: "pts"-}

data VkMessages =  VkMessages { vkMessagesCount :: Int 
                              , vkMessagesItems :: [VkItem]
                              } deriving (Show, Generic)
instance FromJSON VkMessages where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10}

data VkItem = VkItem { vkItemId :: Maybe Int
                     , vkItemFromId :: Int
                     , vkItemText :: String
                     , vkItemAttachments :: [VkAttachment]
                     , vkItemImportant :: Maybe Bool
                     , vkItemGeo :: Maybe VkGeo
                     , vkItemFwdMessages :: Maybe [VkItem]
                     , vkItemPayload :: Maybe String
                     } deriving (Show, Generic)
instance FromJSON VkItem where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6}



data VkAttachment = VkAttachmentPhoto { vkAttachmentPhotoType :: String,
                                       vkAttachmentPhotoPhoto :: VkPhoto
                                      } |
                    VkAttachmentDoc { vkAttachmentDocType :: String
                                    , vkAttachmentDocDoc :: VkDoc
                                    } |
                    VkAttachmentVideo { vkAttachmentVideoType :: String
                                      , vkAttachmentVideoVideo :: VkVideo 
                                      } |
                    VkAttachmentAudio { vkAttachmentAudioType :: String
                                      , vkAttachmentAudioAudio :: VkAudio
                                      } |
                    VkAttachmentWall { vkAttachmentWallType :: String
                                     , vkAttachmentWallWall :: VkWall
                                     } |
                    VkAttachmentMarket { vkAttachmentMarketType :: String
                                       , vkAttachmentMarketMarket :: VkMarket
                                       } |
                    VkAttachmentStory { vkAttachmentStoryType :: String
                                      , vkAttachmentStoryStory :: VkStory
                                      } | 
                    VkAttachmentPoll { vkAttachmentPollType :: String
                                     , vkAttachmentPollPoll :: VkPoll
                                     } |
                    VkAttachmentSticker { vkAttachmentStickerType :: String
                                        , vkAttachmentStickerSticker :: VkSticker
                                        } |
                    VkAttachmentAudioMessage { vkAttachmentAudioMessageType :: String
                                             , vkAttachmentAudioMessageAudioMessage :: VkAudioMessage
                                             } deriving (Show, Generic)
instance FromJSON VkAttachment where
    parseJSON (Object v) = 
        (VkAttachmentPhoto <$> v .: "type"
                           <*> v .: "photo") 
                           <|>
        (VkAttachmentDoc <$> v .: "type"
                         <*> v .: "doc") 
                         <|>
        (VkAttachmentVideo <$> v.: "type"
                           <*> v.: "video") 
                           <|>
        (VkAttachmentAudio <$> v .: "type"
                           <*> v .: "audio")
                           <|>
        (VkAttachmentWall <$> v .: "type"
                          <*> v .: "wall")
                          <|>
        (VkAttachmentMarket <$> v .: "type"
                            <*> v .: "market")
                            <|>
        (VkAttachmentStory <$> v .: "type"
                           <*> v .: "story")
                           <|>
        (VkAttachmentPoll <$> v .: "type"
                          <*> v .: "poll")
                          <|>
        (VkAttachmentSticker <$> v .: "type"
                             <*> v .: "sticker")
                             <|>
        (VkAttachmentAudioMessage <$> v .: "type"
                                  <*> v .: "audio_message")
    parseJSON _ = mzero



data VkPhoto = VkPhoto { vkPhotoId :: Int 
                       , vkPhotoOwnerId :: Int 
                       , vkPhotoAccessKey :: String
                       , vkPhotoText :: String
                       } deriving (Show, Generic)
instance FromJSON VkPhoto where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}

data VkDoc = VkDoc { vkDocId :: Int 
                   , vkDocOwnerId :: Int 
                   , vkDocAccessKey :: String
                   } deriving (Show, Generic)
instance FromJSON VkDoc where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5}

data VkVideo = VkVideo { vkVideoId :: Int 
                       , vkVideoOwnerId :: Int 
                       , vkVideoAccessKey :: String
                       } deriving (Show, Generic)
instance FromJSON VkVideo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}

data VkAudio = VkAudio { vkAudioId :: Int 
                       , vkAudioOwnerId :: Int 
                       } deriving (Show, Generic)
instance FromJSON VkAudio where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}

data VkWall = VkWall { vkWallFromId :: Int
                     , vkWallId :: Int 
                     } deriving (Show, Generic)
instance FromJSON VkWall where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6}

data VkMarket = VkMarket { vkMarketId :: Int 
                         , vkMarketOwnerId :: Int 
                         } deriving (Show, Generic)
instance FromJSON VkMarket where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8}

data VkStory = VkStory { vkStoryId :: Int 
                       , vkStoryOwnerId :: Int
                       } deriving (Show, Generic)
instance FromJSON VkStory where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}

data VkPoll = VkPoll { vkPollId :: Int 
                     , vkPollOwnerId :: Int
                     } deriving (Show, Generic)
instance FromJSON VkPoll where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6}

data VkSticker = VkSticker { vkStickerProductId :: Int 
                           , vkStickerStickerId :: Int 
                           } deriving (Show, Generic)
instance FromJSON VkSticker where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9}

data VkGeo = VkGeo { vkGeoType :: String
                   , vkGeoCoordinates :: VkCoordinates
                   , vkGeoPlace :: VkPlace
                   } deriving (Show, Generic)
instance FromJSON VkGeo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5}

data VkCoordinates = VkCoordinates { vkCoordinatesLatitude :: Double
                                   , vkCoordinatesLongitude :: Double
                                   } deriving (Show, Generic)
instance FromJSON VkCoordinates where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13}

data VkPlace = VkPlace { vkPlaceCountry :: String
                       , vkPlaceCity :: String
                       , vkPlaceTitle :: String
                       } deriving (Show, Generic)
instance FromJSON VkPlace where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7}


data VkAudioMessage = VkAudioMessage { vkAudioMessageId :: Int 
                                     , vkAudioMessageOwnerId :: Int 
                                     , vkAudioMessageAccessKey :: String
                                     } deriving (Show, Generic) 
instance FromJSON VkAudioMessage where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 14}



data VkKeyboard = VkKeyboard { vkKeyboardOneTime :: Bool
                             , vkKeyboardButtons :: [[VkButton]]
                             } deriving (Show, Generic)
instance ToJSON VkKeyboard where
    toJSON  = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10, omitNothingFields = True }

data VkButton = VkButton { vkButtonAction :: VkAction
                         } deriving (Show, Generic)

instance ToJSON VkButton where
    toJSON  = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 , omitNothingFields = True }

data VkAction = VkAction { vkActionType :: String
                         , vkActionLabel :: String
                         , vkActionPayload :: String
                         } deriving (Show, Generic)
instance ToJSON VkAction where
    toJSON  = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8, omitNothingFields = True }