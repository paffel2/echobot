module VkAPI where

--import Config (ConfigModules(help, token))
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (when)
--import Control.Monad.IO.Class
import Data.Aeson (FromJSON(parseJSON), Value, encode)
--import Data.Aeson.Types ( FromJSON(parseJSON), Value, parseEither ) 
import Data.Aeson.Types ( parseEither ) 
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.Text as T
import Logger (Handle, logDebug, logError)
import Network.HTTP.Req
    ( FormUrlEncodedParam
    , GET(GET)
    , JsonResponse
    , MonadHttp(handleHttpException)
    , NoReqBody(NoReqBody)
    , POST(POST)
    , QueryParam(..)
    , ReqBodyUrlEnc(ReqBodyUrlEnc)
    , (/:)
    , (=:)
    , defaultHttpConfig
    , https
    , jsonResponse
    , req
    , responseBody
    , responseStatusCode
    , runReq
    )
--import TelegramResponses
import VkResponses
    ( VkAction(VkAction)
    , VkAttachment(VkAttachmentAudio, VkAttachmentAudioMessage,
             VkAttachmentDoc, VkAttachmentMarket, VkAttachmentPhoto,
             VkAttachmentPoll, VkAttachmentSticker, VkAttachmentStory,
             VkAttachmentVideo, VkAttachmentWall)
    , VkAudio(VkAudio)
    , VkAudioMessage(VkAudioMessage)
    , VkButton(VkButton)
    , VkCoordinates(vkCoordinatesLatitude, vkCoordinatesLongitude)
    , VkDoc(VkDoc)
    , VkGeo(vkGeoCoordinates)
    , VkItem(VkItem, vkItemFromId, vkItemText)
    , VkKeyboard(VkKeyboard)
    , VkMarket(VkMarket)
    , VkMessages(vkMessagesItems)
    , VkPhoto(VkPhoto)
    , VkPoll(VkPoll)
    , VkResponse(VkResponse)
    , VkResponseType(Server)
    , VkSticker(VkSticker)
    , VkStory(VkStory)
    , VkVideo(VkVideo)
    , VkWall(VkWall)
    )

instance MonadHttp IO where
    handleHttpException = throwIO

type VkToken = String

keyboardVk :: VkKeyboard
keyboardVk = VkKeyboard True buttonsVk

b1 :: [VkButton]
b1 = [VkButton (VkAction "text" "1" "1")]

b2 :: [VkButton]
b2 = [VkButton (VkAction "text" "2" "2")]

b3 :: [VkButton]
b3 = [VkButton (VkAction "text" "3" "3")]

b4 :: [VkButton]
b4 = [VkButton (VkAction "text" "4" "4")]

b5 :: [VkButton]
b5 = [VkButton (VkAction "text" "5" "5")]

buttonsVk :: [[VkButton]]
buttonsVk = [b1, b2, b3, b4, b5]

encKeyboard :: T.Text
encKeyboard = T.pack $ BLI.unpackChars (encode keyboardVk)

buildVkPostRequest :: String -> String -> [(T.Text, Maybe T.Text)] -> IO Int
buildVkPostRequest vktoken method param =
    runReq defaultHttpConfig $ do
        r <-
            req
                POST
                (https "api.vk.com" /: "method" /: T.pack method)
                (ReqBodyUrlEnc $ params param)
                jsonResponse
                tokenParam
        return $ responseStatusCode (r :: JsonResponse Value)
  where
    tokenParam =
        buildParams
            [("access_token", T.pack vktoken), ("v", "5.130"), ("random_id", "0")]

params :: [(T.Text, Maybe T.Text)] -> FormUrlEncodedParam
params [] = mempty
params ((a, b):xs) = queryParam a b <> params xs

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams parameters = mconcat $ fmap (uncurry (=:)) parameters

buildVkGetRequest ::
       [Char]
    -> T.Text
    -> [(T.Text, T.Text)]
    -> IO (Either T.Text VkResponseType)
buildVkGetRequest vktoken url parameters = do
    response <- responseBody <$> request
    return $ parseResult response
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
        case parseEither parseJSON r of
            Right (VkResponse result) -> Right result
            Left errMess -> Left $ T.pack errMess

getLongPollServer :: VkToken -> IO (Either T.Text VkResponseType)
getLongPollServer vktoken =
    buildVkGetRequest
        vktoken
        "messages.getLongPollServer"
        [("lp_version", "3"), ("need_pts", "1"), ("v", "5.130")]

getLongPollHistory :: VkToken -> Int -> Int -> IO (Either T.Text VkResponseType)
getLongPollHistory vktoken ts pts =
    buildVkGetRequest
        vktoken
        "messages.getLongPollHistory"
        [("ts", T.pack $ show ts), ("pts", T.pack $ show pts), ("v", "5.130")]

tsAndPts :: VkResponseType -> (Int, Int)
tsAndPts (Server _ _ (Just ts) (Just pts) _ _) = (ts, pts)
tsAndPts _ = (0, 0)

newPts :: Either T.Text VkResponseType -> Int
newPts (Right (Server _ _ _ _ (Just npts) _)) = npts
newPts _ = 0

getTsAndPts :: VkToken -> IO (Either T.Text (Int, Int))
getTsAndPts vktoken = do
    serverInf <- getLongPollServer vktoken
    let ans =
            case serverInf of
                Right (Server _ _ (Just ts) (Just pts) _ _) -> Right (ts, pts)
                Left err -> Left err
                _ -> Left "Unexpected error"
    return ans

createParams :: VkItem -> [(T.Text, Maybe T.Text)]
createParams vkMessage =
    [ ("user_id", Just $ T.pack $ show $ vkItemFromId vkMessage)
    , ("message", Just $ T.pack $ vkItemText vkMessage)
    ]

sendMessageText :: Handle -> VkToken -> VkItem -> IO ()
sendMessageText hLogger vktoken (VkItem _ fromId (x:xs) _ _ _ _ Nothing) =
    when ((fromId > 0) && ((x:xs) /= "/repeat") && ((x:xs) /= "/help")) $ do
        status <- buildVkPostRequest vktoken "messages.send" params'
        if status == 200
            then logDebug hLogger "Message sended"
            else logError hLogger "Message not sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack (x:xs))
        ]
--sendMessageText hLogger token (VkItem _ fromId "" _ _ _ _ Nothing) =
sendMessageText hLogger _ (VkItem _ _ "" _ _ _ _ Nothing) =
    logDebug hLogger "Empty message not sended"
--sendMessageText hLogger token (VkItem _ fromId _ _ _ _ _ (Just _)) =
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
--sendMessageAttachment hLogger vktoken (VkItem _ fromId _ attachments@(x:xs) _ _ _ _) =
sendMessageAttachment hLogger vktoken (VkItem _ fromId _ (x:xs) _ _ _ _) =
    when (fromId > 0) $ do
        let parameters = createParamsAttachment <$> (x:xs)
        status <-
            mapM
                (buildVkPostRequest vktoken "messages.send")
                (fmap (++ [("user_id", Just $ T.pack $ show fromId)]) parameters)
        if all (== 200) status
            then logDebug hLogger "All attachments sended"
            else logError hLogger "One or all attachments not sended"
sendMessageAttachment _ _ (VkItem _ _ _ [] _ _ _ _) = return ()
--sendMessageAttachment hLogger token (VkItem _ fromId _ [] _ _ _ _) = return ()

sendKeyboardVk :: Handle -> VkToken -> VkItem -> IO ()
sendKeyboardVk hLogger vktoken (VkItem _ fromId text _ _ _ _ _) =
    when ((fromId > 0) && (text == "/repeat")) $ do
        status <- buildVkPostRequest vktoken "messages.send" params'
        if status == 200
            then logDebug hLogger "Keyboard sended"
            else logError hLogger "Keyboard  not sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack "Choose number of repetitions")
        , ("keyboard", Just encKeyboard)
        ]

sendGeoVK :: Handle -> VkToken -> VkItem -> IO ()
sendGeoVK hLogger vktoken (VkItem _ fromId _ _ _ (Just geo) _ _) =
    when (fromId > 0) $ do
        status <- buildVkPostRequest vktoken "messages.send" params'
        if status == 200
            then logDebug hLogger "Geo sended"
            else logError hLogger "Geo  not sended"
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("lat", Just $ T.pack $ show lat)
        , ("long", Just $ T.pack $ show long)
        ]
    lat = vkCoordinatesLatitude $ vkGeoCoordinates geo
    long = vkCoordinatesLongitude $ vkGeoCoordinates geo
--sendGeoVK hLogger token (VkItem _ fromId _ _ _ _ _ _) = return ()
sendGeoVK _ _ _ = return ()

findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
findRepeatNumber listOfUsers chatId = do
    let n = lookup chatId listOfUsers
    case n of
        Just x -> do
            putStrLn "user founded"
            return x
        Nothing -> do
            putStrLn "user not found"
            return 1

sendMessageRepeatText ::
       Handle -> String -> [(Int, Int)] -> VkItem -> IO (Maybe (Int, Int))
sendMessageRepeatText hLogger vktoken _ (VkItem _ fromId _ _ _ _ _ (Just button)) =
    if fromId > 0
        then do
            status <- buildVkPostRequest vktoken "messages.send" params'
            if status == 200
                then do
                    logDebug hLogger $
                        T.concat
                            [ "user "
                            , T.pack $ show fromId
                            , " change the number of repetitions to "
                            , T.pack button
                            ]
                    return $ Just (fromId, read button)
                else do
                    logError hLogger "Number of repetitions not changed"
                    return Nothing
        else return Nothing
  where
    params' =
        [ ("user_id", Just $ T.pack $ show fromId)
        , ("message", Just $ T.pack ("the number of repetitions is " ++ button))
        ]
sendMessageRepeatText _ _ _ (VkItem _ _ _ _ _ _ _ Nothing) = return Nothing
{-sendMessageRepeatText hLogger vktoken list (VkItem _ fromId _ _ _ _ _ Nothing) =
    return Nothing-}

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
    if fromId > 0
        then do
            n <- findRepeatNumber list fromId
            repeatMessage' n vktoken item
        else putStr ""
  where
    repeatMessage' 0 _ _ = logDebug hLogger "All sended"
    repeatMessage' x token' item' = do
        sendMessageText hLogger token' item'
        sendMessageAttachment hLogger token' item'
        sendGeoVK hLogger token' item'
        repeatMessage' (x - 1) token' item'
                  --putStr ""

sendMessageHelp :: Handle -> VkToken -> String -> VkItem -> IO ()
sendMessageHelp hLogger vktoken help_message (VkItem _ fromId text _ _ _ _ _) =
    when ((fromId > 0) && (text == "/help")) $ do
        status <- buildVkPostRequest vktoken "messages.send" params'
        if status == 200 then logDebug hLogger "Help message sended"
            else logError hLogger "Help message not sended"
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
    -> Either T.Text VkResponseType
    -> [(Int, Int)]
    -> IO [(Int, Int)]
answer hLogger vktoken help_message (Right (Server _ _ _ _ _ (Just messages))) xs =
    answers hLogger vktoken help_message xs $ vkMessagesItems messages
answer hLogger _ _ (Right _) xs =
    logError hLogger "Unexcepted error" >> return xs
answer hLogger _ _ (Left err) xs = do
    logError hLogger err
    return xs

echo ::
       Handle
    -> VkToken
    -> Maybe Int
    -> Maybe Int
    -> String
    -> [(Int, Int)]
    -> IO ()
echo hLogger vktoken Nothing Nothing help_message listOfUsers = do
    tsPts <- getTsAndPts vktoken
    case tsPts of
        Right (ts, pts) -> do
            updates <- getLongPollHistory vktoken ts pts
            newListOfUsers <-
                answer hLogger vktoken help_message updates listOfUsers
            let npts = newPts updates
            threadDelay 3000000
            echo hLogger vktoken (Just ts) (Just npts) help_message newListOfUsers
        Left _ -> logError hLogger "No pts and ts parameter"
echo hLogger vktoken (Just ts') (Just pts') help_message listOfUsers = do
    updates <- getLongPollHistory vktoken ts' pts'
    newListOfUsers <- answer hLogger vktoken help_message updates listOfUsers
    tsPts <- getTsAndPts vktoken
    case tsPts of
        Right (ts, pts) -> do
            threadDelay 3000000
            echo hLogger vktoken (Just ts) (Just pts) help_message newListOfUsers
        Left _ -> logError hLogger "No pts and ts parameter"
echo hLogger _ _ _ _ _ = logError hLogger "Unexpected error"

