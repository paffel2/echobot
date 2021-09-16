module Telegram.BuildRequest where

import Control.Exception (catch)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson (FromJSON(parseJSON), ToJSON, Value)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Logger (Handle, logError)
import Network.HTTP.Req
    ( QueryParam,
      (/:),
      (=:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      responseStatusCode,
      runReq,
      GET(GET),
      HttpException,
      JsonResponse,
      NoReqBody(NoReqBody),
      POST(POST),
      Req,
      ReqBodyJson(ReqBodyJson) )

import Telegram.Types ( TelegramToken, StatusResult )
import Telegram.Responses (TelegramResponse(TelegramResponse))


type ParametersList = [(T.Text, T.Text)]
type TelegramMethod = String

buildParams :: (QueryParam p, Monoid p) => ParametersList -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params

buildTelegramGetRequest ::
       FromJSON a
    => Handle IO
    -> TelegramToken
    -> TelegramMethod
    -> ParametersList
    -> IO (Maybe a)
buildTelegramGetRequest hLogger tgtoken url params =
    catch
        (runReq defaultHttpConfig $ do
             request <-
                 req
                     GET
                     (https "api.telegram.org" /: T.pack ("bot" ++ tgtoken) /:
                      T.pack url)
                     NoReqBody
                     jsonResponse
                     param :: Req (JsonResponse Value)
             case parseMaybe parseJSON $ responseBody request of
                 Just (TelegramResponse True _ (Just result)) -> do
                     return $ Just result
                 Just (TelegramResponse False (Just _) _) -> do
                     liftIO $ logError hLogger "No response"
                     return Nothing
                 Just (TelegramResponse True _ Nothing) -> do
                     liftIO $ logError hLogger "No result"
                     return Nothing
                 _ -> do
                     liftIO $ logError hLogger "Unexpected error"
                     return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request. "
        return Nothing
  where
    param = buildParams params

buildTelegramPostRequest ::
       ToJSON b
    => Handle IO
    -> TelegramToken
    -> TelegramMethod
    -> b
    -> ParametersList
    -> IO (Maybe StatusResult)
buildTelegramPostRequest hLogger tgtoken url body params =
    catch
        (runReq defaultHttpConfig $ do
             request <-
                 req
                     POST
                     (https "api.telegram.org" /: T.pack ("bot" ++ tgtoken) /:
                      T.pack url)
                     (ReqBodyJson body)
                     jsonResponse
                     param :: Req (JsonResponse Value)
             if responseStatusCode request == 200
                 then liftIO $ return $ Just 200
                 else do
                     liftIO $ logError hLogger "No response"
                     liftIO $ return Nothing) $ \e -> do
        let _ = (e :: HttpException)
        logError hLogger "Bad request. "
        return Nothing
  where
    param = buildParams params





