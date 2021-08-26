{-# LANGUAGE OverloadedStrings #-}

module Vk.API where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Logger (Handle, logDebug, logError)
import Vk.BuildRequests (VkToken, buildVkGetRequest, buildVkPostRequest)
import Vk.Keyboard (encKeyboard)
import Vk.Responses
    ( VkAttachment(VkAttachmentAudio, VkAttachmentAudioMessage,
             VkAttachmentDoc, VkAttachmentMarket, VkAttachmentPhoto,
             VkAttachmentPoll, VkAttachmentSticker, VkAttachmentStory,
             VkAttachmentVideo, VkAttachmentWall)
    , VkAudio(VkAudio)
    , VkAudioMessage(VkAudioMessage)
    , VkCoordinates(vkCoordinatesLatitude, vkCoordinatesLongitude)
    , VkDoc(VkDoc)
    , VkGeo(vkGeoCoordinates)
    , VkItem(VkItem, vkItemFromId, vkItemText)
    , VkMarket(VkMarket)
    , VkMessages(vkMessagesItems)
    , VkPhoto(VkPhoto)
    , VkPoll(VkPoll)
    , VkResponseType(Server)
    , VkSticker(VkSticker)
    , VkStory(VkStory)
    , VkVideo(VkVideo)
    , VkWall(VkWall)
    )

getLongPollServer :: Handle -> VkToken -> IO (Maybe VkResponseType)
getLongPollServer hLogger vktoken =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollServer"
        [("lp_version", "3"), ("need_pts", "1"), ("v", "5.130")]

getLongPollHistory ::
       Handle -> VkToken -> Int -> Int -> IO (Maybe VkResponseType)
getLongPollHistory hLogger vktoken ts pts =
    buildVkGetRequest
        hLogger
        vktoken
        "messages.getLongPollHistory"
        [("ts", T.pack $ show ts), ("pts", T.pack $ show pts), ("v", "5.130")]

getTsAndPts :: Handle -> VkToken -> IO (Maybe (Int, Int))
getTsAndPts hLogger vktoken = do
    serverInf <- getLongPollServer hLogger vktoken
    case serverInf of
        Just (Server _ _ (Just ts) (Just pts) _ _) -> return $ Just (ts, pts)
        _ -> return Nothing --подумать на счет ошибки

createParams :: VkItem -> [(T.Text, Maybe T.Text)]
createParams vkMessage =
    [ ("user_id", Just $ T.pack $ show $ vkItemFromId vkMessage)
    , ("message", Just $ T.pack $ vkItemText vkMessage)
    ]

sendMessageText :: Handle -> VkToken -> VkItem -> IO ()
sendMessageText hLogger vktoken (VkItem _ fromId (x:xs) _ _ _ _ Nothing) =
    when ((fromId > 0) && ((x : xs) /= "/repeat") && ((x : xs) /= "/help")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Message not sended"
            Just _ -> logDebug hLogger "Message sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
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

sendMessageAttachment :: Handle -> VkToken -> VkItem -> IO ()
sendMessageAttachment hLogger vktoken (VkItem _ fromId _ (x:xs) _ _ _ _) =
    when (fromId > 0) $ do
        let parameters = createParamsAttachment <$> (x : xs)
        status <-
            mapM
                (buildVkPostRequest hLogger vktoken "messages.send")
                (fmap (++ [("user_id", Just $ T.pack $ show fromId)]) parameters)
        if all isJust status
            then logDebug hLogger "All attachments sended"
            else logError hLogger "One or all attachments not sended"
sendMessageAttachment _ _ (VkItem _ _ _ [] _ _ _ _) = return ()

sendKeyboardVk :: Handle -> VkToken -> VkItem -> IO ()
sendKeyboardVk hLogger vktoken (VkItem _ fromId text _ _ _ _ _) =
    when ((fromId > 0) && (text == "/repeat")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Keyboard  not sended"
            Just _ -> logDebug hLogger "Keyboard sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack "Choose number of repetitions")
        , ("keyboard", Just encKeyboard)
        ]

sendGeoVK :: Handle -> VkToken -> VkItem -> IO ()
sendGeoVK hLogger vktoken (VkItem _ fromId _ _ _ (Just geo) _ _) =
    when (fromId > 0) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Geo  not sended"
            Just _ -> logDebug hLogger "Geo sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("lat", Just $ T.pack $ show lat)
        , ("long", Just $ T.pack $ show long)
        ]
    lat = vkCoordinatesLatitude $ vkGeoCoordinates geo
    long = vkCoordinatesLongitude $ vkGeoCoordinates geo
sendGeoVK _ _ _ = return ()

{-findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
findRepeatNumber listOfUsers chatId = do
    let n = lookup chatId listOfUsers
    case n of
        Just x -> return x
        Nothing -> return 1-}
findRepeatNumber' :: [(Int, Int)] -> Int -> Int
findRepeatNumber' listOfUsers chatId = fromMaybe 1 $ lookup chatId listOfUsers

sendMessageRepeatText ::
       Handle -> String -> [(Int, Int)] -> VkItem -> IO (Maybe (Int, Int))
sendMessageRepeatText hLogger vktoken _ (VkItem _ fromId _ _ _ _ _ (Just button)) =
    if fromId > 0
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
                            , T.pack $ show fromId
                            , " change the number of repetitions to "
                            , T.pack button
                            ]
                    return $ Just (fromId, read button)
        else return Nothing
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack ("the number of repetitions is " ++ button))
        ]
sendMessageRepeatText _ _ _ (VkItem _ _ _ _ _ _ _ Nothing) = return Nothing

answers ::
       Handle
    -> VkToken
    -> String
    -> [(Int, Int)]
    -> [VkItem]
    -> IO [(Int, Int)]
answers hLogger vktoken help_message list xs = do
    mapM_ (repeatMessage hLogger vktoken list) xs
    mapM_ (sendKeyboardVk hLogger vktoken) xs
    mapM_ (sendMessageHelp hLogger vktoken help_message) xs
    update <- mapM (sendMessageRepeatText hLogger vktoken list) xs
    return $ updateListUsers list update

repeatMessage :: Handle -> VkToken -> [(Int, Int)] -> VkItem -> IO ()
repeatMessage hLogger vktoken list item@(VkItem _ fromId _ _ _ _ _ _) =
    when (fromId > 0) $ do
        repeatMessage' (findRepeatNumber' list fromId) vktoken item
  where
    repeatMessage' 0 _ _ = logDebug hLogger "All sended"
    repeatMessage' x token' item' = do
        sendMessageText hLogger token' item'
        sendMessageAttachment hLogger token' item'
        sendGeoVK hLogger token' item'
        repeatMessage' (x - 1) token' item'

sendMessageHelp :: Handle -> VkToken -> String -> VkItem -> IO ()
sendMessageHelp hLogger vktoken help_message (VkItem _ fromId text _ _ _ _ _) =
    when ((fromId > 0) && (text == "/help")) $ do
        status <- buildVkPostRequest hLogger vktoken "messages.send" params'
        case status of
            Nothing -> logError hLogger "Help message not sended"
            Just _ -> logDebug hLogger "Help message sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack help_message)
        ]

updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
updateListUsers xs (u:us) = updateListUsers newList us
  where
    newList =
        case u of
            Nothing -> xs
            Just (cid, n) -> newlist' ++ [(cid, n)]
                where newlist' = filter ((/= cid) . fst) xs
updateListUsers xs [] = xs

answer ::
       Handle
    -> VkToken
    -> String
    -> Maybe VkResponseType
    -> [(Int, Int)]
    -> IO [(Int, Int)]
answer hLogger vktoken help_message (Just (Server _ _ _ _ _ (Just messages))) xs =
    answers hLogger vktoken help_message xs $ vkMessagesItems messages
answer hLogger _ _ (Just _) xs =
    logError hLogger "Unexcepted error" >> return xs
answer hLogger _ _ Nothing xs = do
    logError hLogger "Unexcepted error"
    return xs
{-buildVkPostRequest ::
       Handle -> String -> String -> [(T.Text, Maybe T.Text)] -> IO (Maybe Int)
buildVkPostRequest hLogger vktoken method param =
    catch
        (do response <- request :: (IO (JsonResponse Value))
            if responseStatusCode response == 200
                then return $ Just 200
                else do
                    logError hLogger "No response"
                    return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request"
        return Nothing
  where
    request =
        req
            POST
            (https "api.vk.com" /: "method" /: T.pack method)
            (ReqBodyUrlEnc $ params param)
            jsonResponse
            tokenParam
    tokenParam =
        buildParams
            [ ("access_token", T.pack vktoken)
            , ("v", "5.130")
            , ("random_id", "0")
            ]

buildVkGetRequest ::
       Handle
    -> String
    -> T.Text
    -> [(T.Text, T.Text)]
    -> IO (Maybe VkResponseType)
buildVkGetRequest hLogger vktoken url parameters =
    catch
        (do response <- responseBody <$> request
            parseResult response) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request"
        return Nothing
  where
    request =
        req
            GET
            (https "api.vk.com" /: "method" /: url)
            NoReqBody
            jsonResponse
            param
    param = buildParams (parameters ++ [("access_token", T.pack vktoken)])
    parseResult r =
        case parseMaybe parseJSON r of
            Just (VkResponse result) -> return $ Just result
            Nothing -> do
                logError hLogger "Unexpected error"
                return Nothing-}
{-echo :: Handle -> VkToken -> String -> [(Int, Int)] -> Int -> Int -> IO ()
echo hLogger vktoken help_message listOfUsers ts pts = do
    updates <- getLongPollHistory hLogger vktoken ts pts
    newListOfUsers <- answer hLogger vktoken help_message updates listOfUsers
    tsPts <- getTsAndPts hLogger vktoken
    case tsPts of
        Just (ts', pts') -> do
            threadDelay 3000000
            echo hLogger vktoken help_message newListOfUsers ts' pts'
        Nothing -> logError hLogger "No pts and ts parameter"-}
