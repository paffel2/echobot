{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.API where

import           Data.Maybe       (isJust)
import qualified Data.Text        as T
import           Logger           (LogHandle, logDebug, logError)
import           UsersLists       (ChatId (getChatId))
import           Vk.BuildRequests (buildVkGetRequest, buildVkPostRequest)
import           Vk.Keyboard      (keyboard)
import           Vk.Responses     (VkAttachment (VkAttachmentAudio, VkAttachmentAudioMessage, VkAttachmentDoc, VkAttachmentMarket, VkAttachmentPhoto, VkAttachmentPoll, VkAttachmentSticker, VkAttachmentStory, VkAttachmentVideo, VkAttachmentWall),
                                   VkAudio (VkAudio),
                                   VkAudioMessage (VkAudioMessage),
                                   VkCoordinates (vkCoordinatesLatitude, vkCoordinatesLongitude),
                                   VkDoc (VkDoc), VkGeo (vkGeoCoordinates),
                                   VkMarket (VkMarket), VkPhoto (VkPhoto),
                                   VkPoll (VkPoll),
                                   VkResponseType (Server, serverPTS, serverTS),
                                   VkSticker (VkSticker), VkStory (VkStory),
                                   VkVideo (VkVideo), VkWall (VkWall))
import           Vk.Types         (Pts (getPts), Ts (getTs), VkToken)

versionParam :: (T.Text, T.Text)
versionParam = ("v", "5.130")

getLongPollServer :: LogHandle IO -> VkToken -> IO (Maybe VkResponseType)
getLongPollServer hLogger vktoken =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollServer"
        [("lp_version", "3"), ("need_pts", "1"), versionParam]

getLongPollHistory ::
       VkToken -> LogHandle IO -> Maybe (Ts, Pts) -> IO (Maybe VkResponseType)
getLongPollHistory vktoken hLogger (Just (ts, pts)) =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollHistory"
        [ ("ts", T.pack $ show $ getTs ts)
        , ("pts", T.pack $ show $ getPts pts)
        , versionParam
        ]
getLongPollHistory _ hLogger Nothing = do
    logError hLogger "No TS and PTS parameters"
    return Nothing

getTsAndPts :: VkToken -> LogHandle IO -> IO (Maybe (Ts, Pts))
getTsAndPts vktoken hLogger = do
    serverInf <- getLongPollServer hLogger vktoken
    case serverInf of
        Just Server {serverTS = (Just ts), serverPTS = (Just pts)} ->
            return $ Just (ts, pts)
        _ -> do
            logError hLogger "Error of getting ts and pts parameters"
            return Nothing

createParamsAttachment :: VkAttachment -> [(T.Text, Maybe T.Text)]
createParamsAttachment (VkAttachmentPhoto (VkPhoto photoId ownerId accessKey _)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "photo" ++ show ownerId ++ "_" ++ show photoId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentDoc (VkDoc docId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "doc" ++ show ownerId ++ "_" ++ show docId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentVideo (VkVideo videoId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "video" ++ show ownerId ++ "_" ++ show videoId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentAudio (VkAudio audioId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "audio" ++ show ownerId ++ "_" ++ show audioId
createParamsAttachment (VkAttachmentWall (VkWall ownerId wallId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "wall" ++ show ownerId ++ "_" ++ show wallId
createParamsAttachment (VkAttachmentMarket (VkMarket marketId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "market" ++ show ownerId ++ "_" ++ show marketId
createParamsAttachment (VkAttachmentStory (VkStory storyId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "story" ++ show ownerId ++ "_" ++ show storyId
createParamsAttachment (VkAttachmentPoll (VkPoll pollId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "poll" ++ show ownerId ++ "_" ++ show pollId
createParamsAttachment (VkAttachmentSticker (VkSticker _ stickerId)) =
    [("sticker_id", Just $ T.pack $ show stickerId)]
createParamsAttachment (VkAttachmentAudioMessage (VkAudioMessage audioId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "audio_message" ++
        show ownerId ++ "_" ++ show audioId ++ "_" ++ accessKey

sendMessageRepeatText :: LogHandle IO -> VkToken -> String -> ChatId -> IO ()
sendMessageRepeatText hLogger vktoken message chatId = do
    status <- buildVkPostRequest hLogger vktoken "messages.send" params
    case status of
        Nothing -> do
            logError hLogger "Number of repetitions not changed"
            return ()
        Just _ -> do
            return ()
  where
    params =
        [ ("user_id", Just . T.pack . show . getChatId $ chatId)
        , ("message", Just . T.pack $ message)
        ]

sendKeyboardVk :: LogHandle IO -> VkToken -> ChatId -> IO ()
sendKeyboardVk hLogger vktoken chatId = do
    status <- buildVkPostRequest hLogger vktoken "messages.send" params
    case status of
        Nothing -> logError hLogger "Keyboard  not sended"
        Just _  -> logDebug hLogger "Keyboard sended"
  where
    params =
        [ ("user_id", Just . T.pack . show . getChatId $ chatId)
        , ("message", Just $ T.pack "Choose number of repetitions")
        , ("keyboard", Just keyboard)
        ]

sendMessageText :: LogHandle IO -> VkToken -> ChatId -> String -> IO ()
sendMessageText hLogger _ _ "" = logDebug hLogger "Empty message not sended"
sendMessageText hLogger vktoken chatId text = do
    status <- buildVkPostRequest hLogger vktoken "messages.send" params
    case status of
        Nothing -> logError hLogger "Message not sended"
        Just _  -> logDebug hLogger "Message sended"
  where
    params =
        [ ("user_id", Just . T.pack . show . getChatId $ chatId)
        , ("message", Just $ T.pack text)
        ]

sendGeoVK :: LogHandle IO -> VkToken -> ChatId -> VkGeo -> IO ()
sendGeoVK hLogger vktoken chatId geo = do
    status <- buildVkPostRequest hLogger vktoken "messages.send" params
    case status of
        Nothing -> logError hLogger "Geo not sended"
        Just _  -> logDebug hLogger "Geo sended"
  where
    params =
        [ ("user_id", Just . T.pack . show . getChatId $ chatId)
        , ("lat", Just $ T.pack $ show lat)
        , ("long", Just $ T.pack $ show long)
        ]
    lat = vkCoordinatesLatitude $ vkGeoCoordinates geo
    long = vkCoordinatesLongitude $ vkGeoCoordinates geo

getAttachmentType :: VkAttachment -> T.Text
getAttachmentType (VkAttachmentPhoto _)        = "Photo"
getAttachmentType (VkAttachmentDoc _)          = "Document"
getAttachmentType (VkAttachmentVideo _)        = "Video"
getAttachmentType (VkAttachmentAudio _)        = "Audio"
getAttachmentType (VkAttachmentWall _)         = "Wall"
getAttachmentType (VkAttachmentMarket _)       = "Market"
getAttachmentType (VkAttachmentStory _)        = "Story"
getAttachmentType (VkAttachmentPoll _)         = "Poll"
getAttachmentType (VkAttachmentSticker _)      = "Sticker"
getAttachmentType (VkAttachmentAudioMessage _) = "AudioMessage"

sendMessageAttachment ::
       LogHandle IO -> VkToken -> ChatId -> [VkAttachment] -> IO ()
sendMessageAttachment hLogger vktoken chatId (x:xs) = do
    let parameters = createParamsAttachment <$> (x : xs)
    status <-
        mapM
            (buildVkPostRequest hLogger vktoken "messages.send")
            (fmap
                 (++ [("user_id", Just . T.pack . show . getChatId $ chatId)])
                 parameters)
    logDebug hLogger (getAttachmentType x <> " attachment sended.")
    if all isJust status
        then logDebug hLogger "Attachments sended"
        else logError hLogger "One or all attachments not sended"
sendMessageAttachment _ _ _ [] = return ()
