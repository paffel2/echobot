module VkAPI where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.IO.Class
import TelegramResponses
import Data.Aeson.Types
import Control.Exception (throwIO) 
import VkResponses
import Data.Foldable
import Control.Concurrent


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

answer (Right (Server _ _ _ _ _ (Just messages))) = answers $ vkMessagesItems messages
answer (Right _) = putStrLn "ошибка 1"
answer (Left err) = putStrLn err

answers [] = putStrLn ""
answers (x:xs) = do
    let p = createParams x
    buildVkPostRequest testTokenVk "messages.send" p
    putStrLn "message send"
    answers xs
    

vkEchoTest :: IO ()
vkEchoTest = do
    updates <- getLongPollHistoryTest
    answer updates
    putStrLn "done"


vkEchoTest' :: VkToken -> Maybe Int -> Maybe Int -> IO ()
vkEchoTest' token Nothing Nothing = do
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts
                    answer updates
                    putStrLn "done"
                    let npts = newPts updates
                    threadDelay 10000000
                    vkEchoTest' token (Just ts) (Just npts)
                  Left err -> putStrLn "бля"

vkEchoTest' token (Just ts') (Just pts') = do
    tsPts <- getTsAndPts token
    case tsPts of Right (ts, pts) -> do
                    updates <- getLongPollHistory token ts pts'
                    answer updates
                    putStrLn "done"
                    let npts = newPts updates
                    threadDelay 2000000 --подумать
                    vkEchoTest' token (Just ts) (Just npts)
                  Left err -> putStrLn "бля"
    {-updates <- getLongPollHistory token ts pts
    answer updates
    putStrLn "done"
    let npts = newPts updates
    vkEchoTest' token (Just ts) (Just npts)-}

