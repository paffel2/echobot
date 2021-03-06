module Vk.BuildRequests where

import           Control.Exception      (catch)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson             (FromJSON (parseJSON), Value)
import           Data.Aeson.Types       (parseMaybe)
import qualified Data.Text              as T
import           Logger                 (LogHandle, logError)
import           Network.HTTP.Req       (FormUrlEncodedParam, GET (GET),
                                         HttpException, JsonResponse,
                                         NoReqBody (NoReqBody), POST (POST),
                                         QueryParam (..), Req,
                                         ReqBodyUrlEnc (ReqBodyUrlEnc),
                                         defaultHttpConfig, https, jsonResponse,
                                         req, responseBody, responseStatusCode,
                                         runReq, (/:), (=:))
import           Vk.Responses           (VkResponse (VkResponse),
                                         VkResponseType)
import           Vk.Types               (VkToken (VkToken))

type PostParams = [(T.Text, Maybe T.Text)]

type GetParams = [(T.Text, T.Text)]

type GetMethod = T.Text

type PostMethod = String

versionParam :: (T.Text, T.Text)
versionParam = ("v", "5.130")

params :: PostParams -> FormUrlEncodedParam
params []          = mempty
params ((a, b):xs) = queryParam a b <> params xs

buildParams :: (QueryParam p, Monoid p) => GetParams -> p
buildParams []         = mempty
buildParams parameters = mconcat $ fmap (uncurry (=:)) parameters

buildVkGetRequest ::
       LogHandle IO
    -> VkToken
    -> GetMethod
    -> GetParams
    -> IO (Maybe VkResponseType)
buildVkGetRequest hLogger (VkToken vktoken) url parameters =
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
                     liftIO $ logError hLogger "Something wrong at get request"
                     return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request"
        return Nothing
  where
    param = buildParams (parameters ++ [("access_token", T.pack vktoken)])

buildVkPostRequest ::
       LogHandle IO -> VkToken -> PostMethod -> PostParams -> IO (Maybe Int)
buildVkPostRequest hLogger (VkToken vktoken) method param =
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
            [("access_token", T.pack vktoken), versionParam, ("random_id", "0")]
