module Vk.BuildRequests where

import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON(parseJSON), Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Logger (Handle, logError)
import Network.HTTP.Req
    ( FormUrlEncodedParam
    , GET(GET)
    , HttpException
    , JsonResponse
    , NoReqBody(NoReqBody)
    , POST(POST)
    , QueryParam(..)
    , Req
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
import Vk.Responses (VkResponse(VkResponse), VkResponseType)

type VkToken = String

params :: [(T.Text, Maybe T.Text)] -> FormUrlEncodedParam
params [] = mempty
params ((a, b):xs) = queryParam a b <> params xs

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams parameters = mconcat $ fmap (uncurry (=:)) parameters

buildVkGetRequest ::
       Handle
    -> String
    -> T.Text
    -> [(T.Text, T.Text)]
    -> IO (Maybe VkResponseType)
buildVkGetRequest hLogger vktoken url parameters =
    catch
        (runReq defaultHttpConfig $ do
             request <-
                 req
                     GET
                     (https "api.vk.com" /: "method" /: url)
                     NoReqBody
                     jsonResponse
                     param :: Req (JsonResponse Value)
             case parseMaybe parseJSON $ responseBody request of
                 Just (VkResponse result) -> return $ Just result
                 Nothing -> do
                     liftIO $ logError hLogger "Unexpected error"
                     return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request"
        return Nothing
  where
    param = buildParams (parameters ++ [("access_token", T.pack vktoken)])

buildVkPostRequest ::
       Handle -> String -> String -> [(T.Text, Maybe T.Text)] -> IO (Maybe Int)
buildVkPostRequest hLogger vktoken method param =
    catch
        (runReq defaultHttpConfig $ do
             request <-
                 req
                     POST
                     (https "api.vk.com" /: "method" /: T.pack method)
                     (ReqBodyUrlEnc $ params param)
                     jsonResponse
                     tokenParam :: Req (JsonResponse Value)
             if responseStatusCode request == 200
                 then return $ Just 200
                 else do
                     liftIO $ logError hLogger "No response"
                     return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request"
        return Nothing
  where
    tokenParam =
        buildParams
            [ ("access_token", T.pack vktoken)
            , ("v", "5.130")
            , ("random_id", "0")
            ]
