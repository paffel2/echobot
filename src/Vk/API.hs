{-# LANGUAGE OverloadedStrings #-}

module Vk.API where

import           Control.Monad    (when)
import           Data.Maybe       (isJust)
import qualified Data.Text        as T
import           Logger           (LogHandle, logDebug, logError)
import           UsersLists       (ChatId (chat_id'), Repeats (Repeats),
                                   RepeatsList,
                                   RepeatsNum (RepeatsNum, repeats_num'),
                                   findRepeatNumber)
import           Vk.BuildRequests (buildVkGetRequest, buildVkPostRequest)
import           Vk.Keyboard      (encKeyboard)
import           Vk.Responses     (VkAttachment (VkAttachmentAudio, VkAttachmentAudioMessage, VkAttachmentDoc, VkAttachmentMarket, VkAttachmentPhoto, VkAttachmentPoll, VkAttachmentSticker, VkAttachmentStory, VkAttachmentVideo, VkAttachmentWall),
                                   VkAudio (VkAudio),
                                   VkAudioMessage (VkAudioMessage),
                                   VkCoordinates (vkCoordinatesLatitude, vkCoordinatesLongitude),
                                   VkDoc (VkDoc), VkGeo (vkGeoCoordinates),
                                   VkItem (VkItem, vkItemFromId, vkItemText),
                                   VkMarket (VkMarket), VkPhoto (VkPhoto),
                                   VkPoll (VkPoll), VkResponseType (Server),
                                   VkSticker (VkSticker), VkStory (VkStory),
                                   VkVideo (VkVideo), VkWall (VkWall))
import           Vk.Types         (HelpMessage (help_mess), Pts (pts'),
                                   Ts (ts'), VkToken)

getLongPollServer :: LogHandle IO -> VkToken -> IO (Maybe VkResponseType)
getLongPollServer hLogger vktoken =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollServer"
        [("lp_version", "3"), ("need_pts", "1"), ("v", "5.130")]

getLongPollHistory ::
       LogHandle IO -> VkToken -> Ts -> Pts -> IO (Maybe VkResponseType)
getLongPollHistory hLogger vktoken ts pts =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollHistory"
        [ ("ts", T.pack $ show $ ts' ts)
        , ("pts", T.pack $ show $ pts' pts)
        , ("v", "5.130")
        ]

getTsAndPts :: LogHandle IO -> VkToken -> IO (Maybe (Ts, Pts))
getTsAndPts hLogger vktoken = do
    serverInf <- getLongPollServer hLogger vktoken
    case serverInf of
        Just (Server _ _ (Just ts) (Just pts) _ _) -> return $ Just (ts, pts)
        _ -> do
            logError hLogger "Error of getting ts and pts parameters"
            return Nothing

createParams :: VkItem -> [(T.Text, Maybe T.Text)]
createParams vkMessage =
    [ ("user_id", Just . T.pack . show . chat_id' . vkItemFromId $ vkMessage)
    , ("message", Just $ T.pack $ vkItemText vkMessage)
    ]

sendMessageText :: LogHandle IO -> VkToken -> VkItem -> IO ()
sendMessageText hLogger vktoken (VkItem _ fromId (x:xs) _ _ _ _ Nothing) =
    when
        ((chat_id' fromId > 0) &&
         ((x : xs) /= "/repeat") && ((x : xs) /= "/help")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Message not sended"
            Just _  -> logDebug hLogger "Message sended"
  where
    params' =
        [ ("user_id", Just . T.pack . show . chat_id' $ fromId)
        , ("message", Just $ T.pack (x : xs))
        ]
sendMessageText hLogger _ (VkItem _ _ "" _ _ _ _ Nothing) =
    logDebug hLogger "Empty message not sended"
sendMessageText hLogger _ (VkItem _ _ _ _ _ _ _ (Just _)) =
    logDebug hLogger "Another type message not sended"

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

sendMessageAttachment :: LogHandle IO -> VkToken -> VkItem -> IO ()
sendMessageAttachment hLogger vktoken (VkItem _ fromId _ (x:xs) _ _ _ _) =
    when (chat_id' fromId > 0) $ do
        let parameters = createParamsAttachment <$> (x : xs)
        status <-
            mapM
                (buildVkPostRequest hLogger vktoken "messages.send")
                (fmap
                     (++ [("user_id", Just . T.pack . show . chat_id' $ fromId)])
                     parameters)
        if all isJust status
            then logDebug hLogger "All attachments sended"
            else logError hLogger "One or all attachments not sended"
sendMessageAttachment _ _ (VkItem _ _ _ [] _ _ _ _) = return ()

sendKeyboardVk :: LogHandle IO -> VkToken -> VkItem -> IO ()
sendKeyboardVk hLogger vktoken (VkItem _ fromId text _ _ _ _ _) =
    when ((chat_id' fromId > 0) && (text == "/repeat")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Keyboard  not sended"
            Just _  -> logDebug hLogger "Keyboard sended"
  where
    params' =
        [ ("user_id", Just . T.pack . show . chat_id' $ fromId)
        , ("message", Just $ T.pack "Choose number of repetitions")
        , ("keyboard", Just encKeyboard)
        ]

sendGeoVK :: LogHandle IO -> VkToken -> VkItem -> IO ()
sendGeoVK hLogger vktoken (VkItem _ fromId _ _ _ (Just geo) _ _) =
    when (chat_id' fromId > 0) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Geo  not sended"
            Just _  -> logDebug hLogger "Geo sended"
  where
    params' =
        [ ("user_id", Just . T.pack . show . chat_id' $ fromId)
        , ("lat", Just $ T.pack $ show lat)
        , ("long", Just $ T.pack $ show long)
        ]
    lat = vkCoordinatesLatitude $ vkGeoCoordinates geo
    long = vkCoordinatesLongitude $ vkGeoCoordinates geo
sendGeoVK _ _ _ = return ()

sendMessageRepeatText ::
       LogHandle IO -> VkToken -> RepeatsList -> VkItem -> IO (Maybe Repeats)
sendMessageRepeatText hLogger vktoken _ (VkItem _ fromId _ _ _ _ _ (Just button)) =
    if chat_id' fromId > 0
        then do
            status <- buildVkPostRequest hLogger vktoken "messages.send" params'
            case status of
                Nothing -> do
                    logError hLogger "Number of repetitions not changed"
                    return Nothing
                Just _ -> do
                    logDebug hLogger $
                        T.concat
                            [ "user "
                            , T.pack . show . chat_id' $ fromId
                            , " change the number of repetitions to "
                            , T.pack . show . repeats_num' $ button
                            ]
                    return $ Just $ Repeats fromId button
        else return Nothing
  where
    params' =
        [ ("user_id", Just . T.pack . show . chat_id' $ fromId)
        , ( "message"
          , Just $
            T.pack
                ("the number of repetitions is " ++
                 (show . repeats_num' $ button)))
        ]
sendMessageRepeatText _ _ _ (VkItem _ _ _ _ _ _ _ Nothing) = return Nothing

repeatMessage :: LogHandle IO -> VkToken -> RepeatsList -> VkItem -> IO ()
repeatMessage hLogger vktoken list item@(VkItem _ fromId _ _ _ _ _ _) =
    when (chat_id' fromId > 0) $ do
        repeatMessage' (findRepeatNumber list fromId) vktoken item
  where
    repeatMessage' (RepeatsNum 0) _ _ = logDebug hLogger "All sended"
    repeatMessage' (RepeatsNum x) token' item' = do
        sendMessageText hLogger token' item'
        sendMessageAttachment hLogger token' item'
        sendGeoVK hLogger token' item'
        repeatMessage' (RepeatsNum (x - 1)) token' item'

sendMessageHelp :: LogHandle IO -> VkToken -> HelpMessage -> VkItem -> IO ()
sendMessageHelp hLogger vktoken help_message (VkItem _ fromId text _ _ _ _ _) =
    when ((chat_id' fromId > 0) && (text == "/help")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Help message not sended"
            Just _  -> logDebug hLogger "Help message sended"
  where
    params' =
        [ ("user_id", Just . T.pack . show . chat_id' $ fromId)
        , ("message", Just $ T.pack $ help_mess help_message)
        ]
