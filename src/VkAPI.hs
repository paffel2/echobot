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

buildVkGetRequest :: [Char] -> [Char] -> T.Text -> [(T.Text, T.Text)] -> IO (Either String VkResponseType )
buildVkGetRequest token groupID url params = do
    response <- responseBody <$> request
    return $ parseResult response
        where request = req
                            GET 
                            (https "api.vk.com" /: "method" /: url)
                            NoReqBody 
                            jsonResponse
                            param
              param = buildParams (params ++ [("access_token", T.pack token)] ++ [("group_id", T.pack groupID)])
              parseResult r = case parseEither parseJSON r of
                  Right (VkResponse result) -> Right result
                  Left errMess -> Left "err2"
 
getLongPollServerTest' = buildVkGetRequest testTokenVk "203142656" "messages.getLongPollServer" [("lp_version","3"),("need_pts","1"),("v","5.130")]