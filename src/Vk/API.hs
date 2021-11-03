{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.API where

import           Data.Maybe       (isJust)
import qualified Data.Text        as T
import           Logger           (LogHandle, logDebug, logError)
import           UsersLists       (ChatId (getChatId),
                                   HelpMessage (getHelpMessage))
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
createParamsAttachment (VkAttachmentPhoto _ (VkPhoto photoId ownerId accessKey _)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "photo" ++ show ownerId ++ "_" ++ show photoId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentDoc _ (VkDoc docId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "doc" ++ show ownerId ++ "_" ++ show docId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentVideo _ (VkVideo videoId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "video" ++ show ownerId ++ "_" ++ show videoId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentAudio _ (VkAudio audioId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "audio" ++ show ownerId ++ "_" ++ show audioId
createParamsAttachment (VkAttachmentWall _ (VkWall ownerId wallId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "wall" ++ show ownerId ++ "_" ++ show wallId
createParamsAttachment (VkAttachmentMarket _ (VkMarket marketId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "market" ++ show ownerId ++ "_" ++ show marketId
createParamsAttachment (VkAttachmentStory _ (VkStory storyId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "story" ++ show ownerId ++ "_" ++ show storyId
createParamsAttachment (VkAttachmentPoll _ (VkPoll pollId ownerId)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr = "poll" ++ show ownerId ++ "_" ++ show pollId
createParamsAttachment (VkAttachmentSticker _ (VkSticker _ stickerId)) =
    [("sticker_id", Just $ T.pack $ show stickerId)]
createParamsAttachment (VkAttachmentAudioMessage "audio_message" (VkAudioMessage audioId ownerId accessKey)) =
    [("attachment", Just $ T.pack attachStr)]
  where
    attachStr =
        "audio_message" ++
        show ownerId ++ "_" ++ show audioId ++ "_" ++ accessKey
createParamsAttachment _ = []

sendMessageHelp :: LogHandle IO -> VkToken -> HelpMessage -> ChatId -> IO ()
sendMessageHelp hLogger vktoken help_message chatId = do
    status <- buildVkPostRequest hLogger vktoken "messages.send" params'
    case status of
        Nothing -> logError hLogger "Help message not sended"
        Just _  -> logDebug hLogger "Help message sended"
  where
    params' =
        [ ("user_id", Just . T.pack . show . getChatId $ chatId)
        , ("message", Just $ T.pack $ getHelpMessage help_message)
        ]

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
    if all isJust status
        then logDebug hLogger "Attachments sended"
        else logError hLogger "One or all attachments not sended"
sendMessageAttachment _ _ _ [] = return ()
