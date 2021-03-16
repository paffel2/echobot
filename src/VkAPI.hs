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
import Control.Exception (throwIO) 
import VkResponses
import Data.Foldable
import Control.Concurrent
import GHC.Generics

import qualified Network.HTTP.Client.MultipartFormData as LM



instance MonadHttp IO where
  handleHttpException = throwIO
type VkToken = String

testTokenVk :: VkToken
testTokenVk = "57787f7aaf82fde6b43ccae4c287835e3123ec05f979f325bb5471e002a11eecb8a4c359368cb053c572b"

getLongPollServerTest :: IO Int 
getLongPollServerTest  = runReq defaultHttpConfig $ do
    r <- req
          GET
          url
          NoReqBody
          jsonResponse
          queryParams
    return $ responseStatusCode (r :: JsonResponse Value)
        where url = https "api.vk.com" /: "method" /: "messages.getLongPollServer"
              params = [("lp_version","3"),("need_pts","1"),("group_id","203142656"),("v","5.130"),("access_token",T.pack testTokenVk)]
              queryParams = buildParams params

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params

{-buildTelegramGetRequest :: FromJSON a => TelegramToken -> String -> [(T.Text, T.Text)] -> IO (Either String a)
buildTelegramGetRequest token url params = do
    response <- responseBody <$> request
    return $ parseResult response
        where request = req
                            GET
                            (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
                            NoReqBody
                            jsonResponse
                            param
              param = buildParams params
              parseResult r = case parseEither parseJSON r of
                Right (TelegramResponse True _ (Just result)) -> Right result
                Right (TelegramResponse False (Just errMess) _) -> Left "1 er"
                Right (TelegramResponse True errMess Nothing) -> Left  "no result"
                Left errMess -> Left "2 er"-}

buildVkGetRequest :: [Char] -> T.Text -> [(T.Text, T.Text)] -> IO (Either String VkResponseType )
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
                  Left errMess -> Left errMess
 
getLongPollServerTest' :: IO (Either String VkResponseType)
getLongPollServerTest' = buildVkGetRequest testTokenVk "messages.getLongPollServer" [("lp_version","3"),("need_pts","1"),("v","5.130")]

getLongPollHistoryTest :: IO (Either String VkResponseType)
getLongPollHistoryTest = buildVkGetRequest testTokenVk "messages.getLongPollHistory" [("ts","1730196697"),("pts","10000147"),("v","5.130")]

getLongPollServer :: VkToken -> IO (Either String VkResponseType)
getLongPollServer token = buildVkGetRequest token "messages.getLongPollServer" [("lp_version","3"),("need_pts","1"),("v","5.130")]

getLongPollHistory :: VkToken -> Int -> Int -> IO (Either String VkResponseType)
getLongPollHistory token ts pts = buildVkGetRequest token "messages.getLongPollHistory" [("ts",T.pack $ show ts),("pts",T.pack $ show pts),("v","5.130")]

tsAndPts :: VkResponseType -> (Int, Int)
tsAndPts (Server _ _ (Just ts) (Just pts) _ _ ) = (ts,pts)

newPts :: Either String VkResponseType -> Int
newPts (Right (Server _ _ _ _ (Just npts) _ )) = npts

getTsAndPts :: VkToken -> IO (Either String (Int, Int))
getTsAndPts token = do
    serverInf <- getLongPollServer token
    let ans = case serverInf of Right (Server _ _ (Just ts) (Just pts) _ _) -> Right (ts,pts)
                                Left err -> Left err
    return ans

{-buildTelegramPostRequest :: ToJSON b => String -> String -> b -> [(T.Text,T.Text)] -> IO Int
buildTelegramPostRequest token url body params = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
        (ReqBodyJson body)
        jsonResponse
        param
    return $ responseStatusCode (r :: JsonResponse Value)
        where param = buildParams params-}

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

--params :: (Monoid p, QueryParam p, http-api-data-0.4.1.1:Web.Internal.HttpApiData.ToHttpApiData a) => [(T.Text, Maybe a)] -> p
params [] = mempty 
params ((a,b):xs) = queryParam a b <> params xs

sendMessageVkTest = buildVkPostRequest testTokenVk "messages.send" [("user_id",Just "30087801"),("message",Just "1")]


createParams :: VkItem  -> [(T.Text, Maybe T.Text)]
createParams vkMessage = [ ("user_id", Just $ T.pack $ show $ vkItemFromId vkMessage)
                         , ("message", Just $ T.pack $ vkItemText vkMessage)
                         ]
--sendMessageText :: String -> VkItem -> IO ()
sendMessageText :: String -> VkItem -> IO ()
{-sendMessageText token (VkItem _ fromId text _ _ _ _)  = do
    let params = [ ("user_id", Just $ T.pack $ show fromId)
                 , ("message", Just $ T.pack $ text)
                 ][ ("user_id", Just $ T.pack $ show fromId)
                 , ("message", Just $ T.pack $ text)
                 ]
    --statusCode <- buildVkPostRequest token "messages.send" params
    buildVkPostRequest token "messages.send" params
    --return statusCode-}
sendMessageText token (VkItem _ fromId text _ _ _ _)  =
    if fromId > 0 then do
        buildVkPostRequest token "messages.send" params'
        putStrLn "message send"
    else
        putStr ""
        where params' = [ ("user_id", Just $ T.pack $ show fromId)
                        , ("message", Just $ T.pack $ text)
                        ]

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

--createParamsGeo :: Maybe VkGeo -> [(T.Text, Maybe T.Text)]
--createParamsGeo (Just geo)
                                            
sendMessageAttachment :: VkToken -> VkItem -> IO ()
sendMessageAttachment token (VkItem _ fromId _ attachments _ _ _) = 
    if fromId > 0 then do
        let params = createParamsAttachment <$> attachments
        mapM_ (buildVkPostRequest token "messages.send") (fmap (++ [("user_id", Just $ T.pack $ show fromId)]) params)--доделать для обработки ошибок
        putStrLn "attsend"
    else putStr ""


answer token (Right (Server _ _ _ _ _ (Just messages))) = answers token $ vkMessagesItems messages
answer token (Right _) = putStrLn "ошибка 1"
answer token (Left err) = putStrLn err

answers :: VkToken -> [VkItem] -> IO ()
answers _ [] = putStrLn "all sended"
answers token xs = do
    mapM_ (sendMessageText token) xs
    mapM_ (sendMessageAttachment token) xs
    putStrLn "all sended"

    
    

vkEchoTest :: IO ()
vkEchoTest = do
    updates <- getLongPollHistoryTest
    answer testTokenVk updates
    putStrLn "done"


vkEchoTest' :: VkToken -> Maybe Int -> Maybe Int -> IO ()
vkEchoTest' token Nothing Nothing = do
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts
                    answer token updates
                    let npts = newPts updates
                    threadDelay 3000000
                    vkEchoTest' token (Just ts) (Just npts)
                  Left err -> putStrLn "бля"

vkEchoTest' token (Just ts') (Just pts') = do
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts'
                    answer token updates
                    let npts = newPts updates
                    threadDelay 3000000 --подумать
                    vkEchoTest' token (Just ts) (Just npts)
                  Left err -> putStrLn "бля"
    {-updates <- getLongPollHistory token ts pts
    answer updates
    putStrLn "done"
    let npts = newPts updates
    vkEchoTest' token (Just ts) (Just npts)-}

t :: IO ()
t = vkEchoTest' testTokenVk Nothing Nothing


{-keyboardVk = VkKeyboard True buttonsVk False

b1 = VkButton (VkAction "text" "1" "{\"button\": \"1\"}")-- "secondary"

b2 = VkButton (VkAction "text" "2" "{\"button\": \"2\"}")-- "secondary"

b3 = VkButton (VkAction "text" "3" "{\"button\": \"3\"}")-- "secondary"

b4 = VkButton (VkAction "text" "4" "{\"button\": \"4\"}")-- "secondary"

b5 = VkButton (VkAction "text" "5" "{\"button\": \"5\"}")-- "secondary"

buttonsVk = [b1, b2, b3, b4, b5]
encKeyboard = T.pack $ BLI.unpackChars (encode keyboardVk)
a = TIO.putStrLn encKeyboard-}
sendKeyboardTest = buildVkPostRequest testTokenVk "messages.send" [("user_id", Just "30087801"),("message", Just "выбирай")]--,("keyboard", Just keyboardVk)]



--sendKeyboardVk ::IO Int 
sendKeyboardVk :: IO ()
sendKeyboardVk = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.vk.com" /: "method" /: "messages.send")
        (ReqBodyUrlEnc $ params [("keyboard",Just keytext)])
        jsonResponse 
        params'
    liftIO $ print (responseBody r :: Value)
    --return $ responseStatusCode (r :: JsonResponse Value)
        where params' = buildParams [("user_id","30087801"),("message","тест"),("access_token", T.pack testTokenVk),("v","5.130"),("random_id","0")]

--keyboardParam = queryParam "keyboard" _a






smallKeyBoard :: VkKeyboard
smallKeyBoard = VkKeyboard True [[smallButton]]
smallButton :: VkButton
smallButton = VkButton (VkAction "text" "test")

keytext = T.pack $ BLI.unpackChars (encode smallKeyBoard)
a = TIO.putStrLn keytext
--a = queryParam "keyboard" (Just keyboardVk)
--a = TIO.readFile "src/VkKeyboard.json"
{-sendForwardMessage :: IO  Int
sendForwardMessage = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.vk.com" /: "method" /: "messages.send")
        (ReqBodyJson forward)
        jsonResponse
        params
    return $ responseStatusCode (r :: JsonResponse Value)
        where
            forward = Forward 81958221 30087801 [50443] [288]
            params = buildParams [("access_token", T.pack testTokenVk),("v","5.130"),("random_id","0")]


{-buildVkPostRequest :: String -> String -> [(T.Text, Maybe T.Text)] -> IO  Int
buildVkPostRequest token method param  = runReq defaultHttpConfig $ do
    r <- req
        POST 
        (https "api.vk.com" /: "method" /: T.pack method)
        (ReqBodyUrlEnc $ params param)
        jsonResponse 
        tokenParam
    return $ responseStatusCode (r :: JsonResponse Value)
        where tokenParam = buildParams [("access_token", T.pack token),("v","5.130"),("random_id","0")]-}
data Forward = Forward { forwardOwnerId :: Int 
                       , forwardPeerId :: Int 
                       , forwardConversationMessageIds :: [Int]
                       , forwardMessageIds :: [Int]
                       } deriving (Show, Generic)
instance ToJSON Forward where
    toJSON  = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 7, omitNothingFields = True  }-}





costul = "%7B%20%22one_time%22%20%3A%20true%2C%0A%20%20%20%20%22inline%22%20%3A%20false%2C%0A%20%20%20%20%22buttons%22%3A%5B%0A%20%20%20%20%20%20%20%20%5B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22action%22%3A%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22type%22%3A%22text%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22label%22%3A%221%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%221%5C%22%7D%22%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%5D%2C%0A%20%20%20%20%20%20%20%20%5B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22action%22%3A%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22type%22%3A%22text%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22label%22%3A%222%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%222%5C%22%7D%22%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%5D%2C%0A%20%20%20%20%20%20%20%20%5B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22action%22%3A%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22type%22%3A%22text%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22label%22%3A%223%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%223%5C%22%7D%22%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%5D%2C%0A%20%20%20%20%20%20%20%20%5B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22action%22%3A%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22type%22%3A%22text%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22label%22%3A%224%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%224%5C%22%7D%22%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%5D%2C%0A%20%20%20%20%20%20%20%20%5B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22action%22%3A%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22type%22%3A%22text%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22label%22%3A%225%22%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%22payload%22%3A%22%7B%5C%22button%5C%22%3A%20%5C%225%5C%22%7D%22%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%5D%0A%20%20%20%20%5D%0A%7D"