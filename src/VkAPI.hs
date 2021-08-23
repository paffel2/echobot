module VkAPI where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.IO.Class
import TelegramResponses
import Data.Aeson.Types
import Control.Exception
import VkResponses
import Data.Foldable
import Control.Concurrent
import GHC.Generics
import Logger
import Config
import Control.Monad




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



buildVkPostRequest :: String -> String -> [(T.Text, Maybe T.Text)] -> IO  Int
buildVkPostRequest token method param  = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.vk.com" /: "method" /: T.pack method)
        (ReqBodyUrlEnc $ params param)
        jsonResponse
        tokenParam
    return $ responseStatusCode (r :: JsonResponse Value)
        where tokenParam = buildParams [("access_token", T.pack token),("v","5.130"),("random_id","0")]


params :: [(T.Text, Maybe T.Text)] -> FormUrlEncodedParam
params [] = mempty
params ((a,b):xs) = queryParam a b <> params xs



buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params


buildVkGetRequest :: [Char] -> T.Text -> [(T.Text, T.Text)] -> IO (Either T.Text VkResponseType )
buildVkGetRequest token url params = do
    response <- responseBody <$> request
    return $ parseResult response
        where request = req
                            GET
                            (https "api.vk.com" /: "method" /: url)
                            NoReqBody
                            jsonResponse
                            param
              param = buildParams (params ++ [("access_token", T.pack token)])
              parseResult r = case parseEither parseJSON r of
                  Right (VkResponse result) -> Right result
                  Left errMess -> Left $ T.pack errMess

getLongPollServer :: VkToken -> IO (Either T.Text VkResponseType)
getLongPollServer token = buildVkGetRequest token "messages.getLongPollServer" [("lp_version","3"),("need_pts","1"),("v","5.130")]

getLongPollHistory :: VkToken -> Int -> Int -> IO (Either T.Text VkResponseType)
getLongPollHistory token ts pts = buildVkGetRequest token "messages.getLongPollHistory" [("ts",T.pack $ show ts),("pts",T.pack $ show pts),("v","5.130")]

tsAndPts :: VkResponseType -> (Int, Int)
tsAndPts (Server _ _ (Just ts) (Just pts) _ _ ) = (ts,pts)
tsAndPts _ = (0,0) --подумать

newPts :: Either T.Text VkResponseType -> Int
newPts (Right (Server _ _ _ _ (Just npts) _ )) = npts
newPts _ = 0 -- подумать

getTsAndPts :: VkToken -> IO (Either T.Text (Int, Int))
getTsAndPts token = do
    serverInf <- getLongPollServer token
    let ans = case serverInf of Right (Server _ _ (Just ts) (Just pts) _ _) -> Right (ts,pts)
                                Left err -> Left err
                                _ -> Left "Unexpected error"
    return ans




createParams :: VkItem  -> [(T.Text, Maybe T.Text)]
createParams vkMessage = [ ("user_id", Just $ T.pack $ show $ vkItemFromId vkMessage)
                         , ("message", Just $ T.pack $ vkItemText vkMessage)
                         ]
sendMessageText :: Handle -> VkToken -> VkItem -> IO ()
sendMessageText hLogger token (VkItem _ fromId text@(x:xs) _ _ _ _ Nothing)  =
    when ((fromId > 0) && (text /="/repeat")&&(text /="/help")) $ do
    status <- buildVkPostRequest token "messages.send" params'
    if status == 200 then
        logDebug  hLogger "Message sended"
    else
        logError hLogger "Message not sended"
        where params' = [ ("user_id", Just $ T.pack $ show fromId)
                        , ("message", Just $ T.pack text)
                        ]
sendMessageText hLogger token (VkItem _ fromId "" _ _ _ _ Nothing)  = logDebug hLogger "Empty message not sended"
sendMessageText hLogger token (VkItem _ fromId _ _ _ _ _ (Just _)) = logDebug hLogger "Another type message not sended"


createParamsAttachment :: VkAttachment -> [(T.Text, Maybe T.Text)]
createParamsAttachment (VkAttachmentPhoto _ (VkPhoto photoId ownerId accessKey _)) = [("attachment", Just $ T.pack attachStr)]
                                                                                        where
                                                                                            attachStr = "photo" ++ show ownerId ++ "_" ++ show photoId ++ "_" ++ accessKey

createParamsAttachment (VkAttachmentDoc _ (VkDoc docId ownerId accessKey)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "doc" ++ show ownerId ++ "_" ++ show docId ++ "_" ++ accessKey

createParamsAttachment (VkAttachmentVideo _ (VkVideo videoId ownerId accessKey)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "video" ++ show ownerId ++ "_" ++ show videoId ++ "_" ++ accessKey
createParamsAttachment (VkAttachmentAudio _ (VkAudio audioId ownerId)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "audio" ++ show ownerId ++ "_" ++ show audioId
createParamsAttachment (VkAttachmentWall _ (VkWall ownerId wallId)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "wall" ++ show ownerId ++ "_" ++ show wallId
createParamsAttachment (VkAttachmentMarket _ (VkMarket marketId ownerId)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "market" ++ show ownerId ++ "_" ++ show marketId
createParamsAttachment (VkAttachmentStory _ (VkStory storyId ownerId)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "story" ++ show ownerId ++ "_" ++ show storyId
createParamsAttachment (VkAttachmentPoll _ (VkPoll pollId ownerId)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                      attachStr = "poll" ++ show ownerId ++ "_" ++ show pollId
createParamsAttachment (VkAttachmentSticker _ (VkSticker _ stickerId)) = [("sticker_id", Just $ T.pack $ show stickerId)]

createParamsAttachment (VkAttachmentAudioMessage "audio_message" (VkAudioMessage audioId ownerId accessKey)) = [("attachment", Just $ T.pack attachStr)]
                                                                                  where
                                                                                     attachStr = "audio_message" ++ show ownerId ++ "_" ++ show audioId ++ "_" ++ accessKey
createParamsAttachment _ = []





sendMessageAttachment :: Handle -> VkToken -> VkItem -> IO ()
sendMessageAttachment hLogger token (VkItem _ fromId _ attachments@(x:xs) _ _ _ _) =
    when (fromId > 0) $ do
    let params = createParamsAttachment <$> attachments
    status <- mapM (buildVkPostRequest token "messages.send") (fmap (++ [("user_id", Just $ T.pack $ show fromId)]) params)--доделать для обработки ошибок
    if all (==200) status then
        logDebug hLogger "All attachments sended"
    else logError hLogger "One or all attachments not sended"
sendMessageAttachment hLogger token (VkItem _ fromId _ [] _ _ _ _) = return ()

sendKeyboardVk :: Handle -> VkToken -> VkItem -> IO ()
sendKeyboardVk hLogger token (VkItem _ fromId text _ _ _ _ _) =
    when ((fromId > 0) && (text == "/repeat")) $ do
    status <- buildVkPostRequest token "messages.send" params'
    if status == 200 then
        logDebug hLogger "Keyboard sended"
    else logError hLogger "Keyboard  not sended"
        where params' = [("user_id", Just $ T.pack $ show fromId),("message", Just $ T.pack "выбирай"), ("keyboard", Just encKeyboard)]


sendGeoVK :: Handle -> VkToken -> VkItem -> IO ()
sendGeoVK hLogger token (VkItem _ fromId _ _ _ (Just geo) _ _) =
    when (fromId > 0) $ do
    status <- buildVkPostRequest token "messages.send" params'
    if status == 200 then
        logDebug hLogger "Geo sended"
    else logError hLogger "Geo  not sended"
        where params' = [("user_id", Just $ T.pack $ show fromId),("lat", Just $ T.pack $ show lat), ("long", Just $ T.pack $ show long)]
              lat = vkCoordinatesLatitude $ vkGeoCoordinates geo
              long = vkCoordinatesLongitude $ vkGeoCoordinates geo
sendGeoVK hLogger token (VkItem _ fromId _ _ _ _ _ _) = return ()






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

sendMessageRepeatText :: Handle -> String -> [(Int,Int)] ->  VkItem -> IO (Maybe (Int,Int))
sendMessageRepeatText hLogger token list (VkItem _ fromId _ _ _ _ _ (Just button)) =
    if fromId > 0 then do
        status <- buildVkPostRequest token "messages.send" params'
        if status == 200 then do
            logDebug hLogger $ T.concat ["user " ,T.pack $ show fromId, " change the number of repetitions to ", T.pack button]
            return $ Just (fromId,read button)
        --putStrLn $ "user " ++ show fromId ++ " changed the number of repetitions to " ++ button
        --return $ Just (fromId,read button)
        else do
            logError hLogger "Number of repetitions not changed"
            return  Nothing
    else
        return Nothing
        where params' = [ ("user_id", Just $ T.pack $ show fromId)
                        , ("message", Just $ T.pack ("the number of repetitions is " ++ button))
                        ]
sendMessageRepeatText hLogger token list (VkItem _ fromId _ _ _ _ _ Nothing) = return Nothing

answers :: Handle -> VkToken -> String -> [(Int,Int)] -> [VkItem] -> IO [(Int,Int)]
answers hLogger token help_message list xs = do
    --logDebug hLogger $ T.concat ["numbers of messages = ", T.pack $ show $ length list]
    --print $ length list
    mapM_ (repeatMessage hLogger token list) xs
    mapM_ (sendKeyboardVk hLogger token) xs
    mapM_ (sendMessageHelp token help_message) xs
    update <- mapM (sendMessageRepeatText hLogger token list) xs
    return $ updateListUsers list update
--answers hLogger _ _ list [] = return list

{-answer' :: VkToken -> Either String VkResponseType  -> [(Int,Int)] -> IO [(Int, Int)]
answer' token (Right (Server _ _ _ _ _ (Just messages))) xs = answers' token xs $ vkMessagesItems messages
answer' token (Right _) xs = putStrLn "ошибка 1" >> return xs
answer' token (Left err) xs = putStrLn err >> return xs-}





repeatMessage :: Handle -> VkToken -> [(Int,Int)] -> VkItem -> IO ()
repeatMessage hLogger token list item@(VkItem _ fromId _ _ _ _ _ _) = if fromId > 0 then do
    n <- findRepeatNumber list fromId
    repeatMessage' n token item
    else putStr ""
        where repeatMessage' 0 _ _ = logDebug hLogger "All sended"
              repeatMessage' x token' item' = do
                  sendMessageText hLogger token' item'
                  sendMessageAttachment hLogger token' item'
                  sendGeoVK hLogger token' item'
                  repeatMessage' (x-1) token' item'
                  --putStr ""



sendMessageHelp :: VkToken -> String -> VkItem -> IO ()
sendMessageHelp token help_message (VkItem _ fromId text _ _ _ _ _ )  =
    if  (fromId > 0) && (text=="/help")  then do
        buildVkPostRequest token "messages.send" params'
        putStrLn "help message send"
    else
        putStr ""
        where params' = [ ("user_id", Just $ T.pack $ show fromId)
                        , ("message", Just $ T.pack help_message)
                        ]



updateListUsers ::[(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
updateListUsers xs (u:us) = updateListUsers newList us where
    newList = case u of
        Nothing -> xs
        Just (cid, n) -> newlist' ++ [(cid, n)] where
                              newlist' = filter ((/=cid) . fst) xs
updateListUsers xs [] = xs










{-vkEchoTest :: Handle -> VkToken -> Maybe Int -> Maybe Int -> [(Int,Int)] -> IO ()
vkEchoTest hLogger token Nothing Nothing listOfUsers = do
    logInfo hLogger "Bot started"
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts
                    newListOfUsers <- answer hLogger token updates listOfUsers
                    let npts = newPts updates
                    threadDelay 3000000
                    vkEchoTest hLogger token (Just ts) (Just npts) newListOfUsers
                  Left err -> logError hLogger "No pts and ts parameter"

vkEchoTest hLogger token (Just ts') (Just pts') listOfUsers = do
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts'
                    newListOfUsers <- answer hLogger token updates listOfUsers
                    let npts = newPts updates
                    threadDelay 3000000
                    vkEchoTest hLogger token (Just ts) (Just npts) newListOfUsers
                  Left err -> logError hLogger "No pts and ts parameter"
vkEchoTest hLogger _ _ _ _ = logError hLogger "Unexpected error"-}



answer :: Handle -> VkToken -> String -> Either T.Text VkResponseType  -> [(Int,Int)] -> IO [(Int, Int)]
answer hLogger token help_message (Right (Server _ _ _ _ _ (Just messages))) xs =do
    --logDebug hLogger "answers function started"
    print $ vkMessagesCount messages
    answers hLogger token help_message xs $ vkMessagesItems messages
answer hLogger _ _ (Right _) xs = logError hLogger "Unexcepted error" >> return xs
answer hLogger _ _ (Left err) xs = do
                                    logError hLogger err
                                    return xs




echo :: Handle -> VkToken -> Maybe Int -> Maybe Int -> String -> [(Int,Int)]-> IO ()
echo hLogger token Nothing Nothing help_message listOfUsers = do
    tsPts <- getTsAndPts token
    case tsPts of 
        Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts
                    newListOfUsers <- answer hLogger token help_message updates listOfUsers
                    let npts = newPts updates
                    threadDelay 3000000
                    echo hLogger token (Just ts) (Just npts) help_message newListOfUsers
        Left err -> logError hLogger "No pts and ts parameter"

echo hLogger token (Just ts') (Just pts') help_message listOfUsers = do
        updates <- getLongPollHistory token ts' pts'
        newListOfUsers <- answer hLogger token help_message updates listOfUsers
        tsPts <- getTsAndPts token
        case tsPts of 
                Right (ts,pts) -> do
                    threadDelay 3000000
                    echo  hLogger token (Just ts) (Just pts) help_message newListOfUsers

                Left err -> logError hLogger "No pts and ts parameter"
echo hLogger _ _ _ _ _ = logError hLogger "Unexpected error"




startVkBot' :: Handle -> ConfigModules -> IO ()
startVkBot' hLogger botConf= catch (do
    logInfo hLogger "Bot started"
    echo hLogger (Config.token botConf) Nothing Nothing (Config.help botConf) []) $ \e -> do
        let err = e :: HttpException
        logError hLogger "Bad token"



