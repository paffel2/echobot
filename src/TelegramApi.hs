
{-# LANGUAGE OverloadedStrings #-}
module TelegramApi where


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import Network.HTTP.Req
import Control.Monad.IO.Class
import TelegramResponses
import Data.Aeson.Types
import Control.Exception
import Logger
import Config

instance MonadHttp IO where
    handleHttpException = throwIO

type TelegramToken = String
testToken :: TelegramToken
testToken = "1431530804:AAH5bSr9xr8o3WQlF55hnmpYZYtktn-rzWY"

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params

buildTelegramGetRequest :: FromJSON a => Handle -> TelegramToken -> String -> [(T.Text, T.Text)] -> IO (Either T.Text a)
buildTelegramGetRequest hLogger token url params = do
    response <- responseBody <$> request
    parseResult response

        where request = req
                            GET
                            (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
                            NoReqBody
                            jsonResponse
                            param
              param = buildParams params
              parseResult r = case parseEither parseJSON r of
                Right (TelegramResponse True _ (Just result)) -> do
                                                                    return $ Right result
                Right (TelegramResponse False (Just errMess) _) -> do
                                                                    logError hLogger "No response"
                                                                    return $ Left "No response"
                Right (TelegramResponse True errMess Nothing) -> do
                                                                    logError hLogger "No result"
                                                                    return $ Left "No result"
                _ -> do
                    logError hLogger "Unexpected errorr"
                    return $ Left "Unexpected errorr"

getUpdates :: FromJSON a => Handle ->
    TelegramToken -> Maybe Int -> IO (Either T.Text a)
getUpdates hLogger token (Just updId) = buildTelegramGetRequest hLogger token "getUpdates" [("offset", T.pack $ show updId), ("timeout", "10")]
getUpdates hLogger token Nothing = buildTelegramGetRequest hLogger token "getUpdates" [("offset", "0"), ("timeout", "10")]




echo :: Handle -> TelegramToken -> Maybe Int -> String -> [(Int,Int)]-> IO ()
echo hLogger token updateId help_message listOfUsers = do
  updates <- getUpdates hLogger token updateId
  b <- answers hLogger help_message token updates listOfUsers
  let newlistOfUsers = updateListUsers listOfUsers b
  nextUpdateID <- getLastUpdateId hLogger updates
  echo hLogger token nextUpdateID help_message newlistOfUsers



startTelegramBot' :: Handle -> ConfigModules -> IO ()
startTelegramBot' hLogger botConf = catch (do
    logInfo hLogger "Bot Start"
    echo hLogger (Config.token botConf) (Just 0) (Config.help botConf) []) $ \e -> do
        let err = e :: HttpException
        logError hLogger "Bad token"


getLastUpdateId :: Handle -> Either T.Text [TelegramUpdate] -> IO (Maybe Int)
getLastUpdateId hLogger updates = case updates of
    Left mes-> do
        return Nothing
    Right [] -> do
        logInfo hLogger "No updates"
        return Nothing
    Right xs -> return $ Just $ (+ 1) $ telegramUpdateId $ last xs


answers :: Handle -> String -> TelegramToken ->
    Either T.Text [TelegramUpdate]
    -> [(Int, Int)] -> IO [Maybe (Int, Int)]
answers hLogger help_message token (Right upd) list = mapM (answer' hLogger help_message token list) upd
answers hLogger _ _ _ _ = do
    logError hLogger "Something wrong"
    return [Nothing]


updateListUsers :: [(Int,Int)] -> [Maybe (Int,Int)] -> [(Int,Int)]
updateListUsers xs (u:us) = updateListUsers newList us where
    newList = case u of
        Nothing -> xs
        Just (cid, n) -> newlist' ++ [(cid, n)] where
                              newlist' = filter ((/=cid) . fst) xs
updateListUsers xs [] = xs


findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
findRepeatNumber listOfUsers chatId = do
  let n = lookup chatId listOfUsers
  case n of
    Just x -> do
                 return x
    Nothing -> do
                 return 1

buttons :: [[TelegramInlineKeyboardButton]]
buttons =  [ [TelegramInlineKeyboardButton x x] | x <- ["1","2","3","4","5"]]

keyboard :: TelegramInlineKeyboardMarkup
keyboard = TelegramInlineKeyboardMarkup buttons

answer' :: Handle -> String -> TelegramToken -> [(Int, Int)] -> TelegramUpdate -> IO (Maybe (Int, Int))
answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    n <- findRepeatNumber list chatId
    status <- sendKeyboard' hLogger token chatId
    case status of
        Left txt -> logError hLogger "Keyboard not send"
        Right i -> logDebug hLogger "Keyboard sended"
    return Nothing
        where chatId = telegramChatId $ telegramMessageChat message

answer' hLogger help_message token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    status <- sendMessage' hLogger token chatId help_message Nothing
    case status of
        Left txt -> logError hLogger "Help message not sended"
        Right i -> logDebug hLogger "Help message sended"
    return Nothing
        where chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just text) _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    n <- findRepeatNumber list chatId
    repeatSendMessage' hLogger n token chatId ansText entities
    return Nothing
        where ansText = text
              entities = telegramMessageEntities message
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ (Just anim) _ _ _ _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendAnimation' hLogger n token chatId animid cap
    return Nothing
        where cap = telegramMessageCaption message
              animid = telegramAnimationFileId anim
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just audio) _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendAudio' hLogger n token chatId audioid cap
    return Nothing
        where cap = telegramMessageCaption message
              audioid = telegramAudioFileId audio
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ Nothing _ (Just doc) _ _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendDocument' hLogger n token chatId docid cap
    return Nothing
        where cap = telegramMessageCaption message
              docid = telegramDocumentFileId doc
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (photo:_)) _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendPhoto' hLogger n token chatId photoid cap
    return Nothing
        where cap = telegramMessageCaption message
              photoid = telegramPhotoSizeFileId photo
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just video) _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVideo' hLogger n token chatId videoid cap
    return Nothing
        where cap = telegramMessageCaption message
              videoid = telegramVideoFileId video
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just sticker) _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendSticker' hLogger n token chatId stickerid
    return Nothing
        where stickerid = telegramStickerFileId sticker
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just videoNote) _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVideoNote' hLogger n token chatId videoid
    return Nothing
        where videoid = telegramVideoNoteFileId videoNote
              chatId = telegramChatId $ telegramMessageChat message


answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just voice) _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVoice' hLogger n token chatId voiceid cap
    return Nothing
        where cap = telegramMessageCaption message
              voiceid = telegramVoiceFileId voice
              chatId = telegramChatId $ telegramMessageChat message

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _(Just contact) _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendContact' hLogger n token chatId phoneNumber fname lname vcard
    return Nothing
      where chatId = telegramChatId $ telegramMessageChat message
            phoneNumber = telegramContactPhoneNumber contact
            fname = telegramContactFirstName contact
            lname = telegramContactLastName contact
            vcard = telegramContactVcard contact

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just location) Nothing)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendLocation' hLogger n token chatId lat long horac lp hea par
    return Nothing
      where chatId = telegramChatId $ telegramMessageChat message
            lat = telegramLocationLatitude location
            long = telegramLocationLongitude location
            horac = telegramLocationHorizontalAccuracy location
            lp = telegramLocationLivePeriod location
            hea = telegramLocationHeading location
            par = telegramLocationProximityAlertRadius location

answer' hLogger _ token list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just venue))) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVenue' hLogger n token chatId lat long title address fsid fstype gpid gptype
    return Nothing
      where chatId = telegramChatId $ telegramMessageChat message
            lat =  telegramLocationLatitude $ telegramVenueLocation venue
            long = telegramLocationLongitude $ telegramVenueLocation venue
            title = telegramVenueTitle venue
            address = telegramVenueAddress venue
            fsid = telegramVenueFoursquareId venue
            fstype = telegramVenueFoursquareType venue
            gpid = telegramVenueGooglePlaceId venue
            gptype = telegramVenueGooglePlaceType venue


answer' hLogger _ token list (TelegramUpdate _ _(Just callback@(TelegramCallbackQuery _ user (Just message) _ (Just dat)))) = do
    status <- sendMessage' hLogger token chatId text Nothing
    case status of
        Left txt -> do
            logError hLogger "Keyboard not sended"
            return Nothing
        Right n -> do
            logDebug hLogger "Keyboard sended"
            return $ Just(chatId, read dat :: Int)
        where chatId = telegramUserId user
              text = "Number of reapeting " ++ dat
answer' hLogger _ _ _ _ =
    return Nothing

buildTelegramPostRequest' :: ToJSON b => Handle -> String -> String -> b -> [(T.Text,T.Text)] -> IO (Either T.Text Int)
buildTelegramPostRequest' hLogger token url body params = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
        (ReqBodyJson body)
        jsonResponse
        param
    if responseStatusCode (r :: JsonResponse Value) == 200 then
        return $ Right 200
    else return $ Left "No response"
        where param = buildParams params


sendMessage' :: Handle -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Either T.Text Int)
sendMessage' hLogger token chatId text ent = buildTelegramPostRequest' hLogger token "sendMessage" (TelegramSendMessage chatId text ent Nothing) []

sendAnimation' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendAnimation' hLogger token chatId anim cap = buildTelegramPostRequest' hLogger token "sendAnimation" (TelegramSendAnimation chatId anim cap) []

sendAudio' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendAudio' hLogger token chatId audio cap = buildTelegramPostRequest' hLogger token "sendAudio" (TelegramSendAudio chatId audio cap) []

sendDocument' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendDocument' hLogger token chatId doc cap = buildTelegramPostRequest' hLogger token "sendDocument" (TelegramSendDocument chatId doc cap) []

sendPhoto' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendPhoto' hLogger token chatId photo cap = buildTelegramPostRequest' hLogger token "sendPhoto" (TelegramSendPhoto chatId photo cap) []

sendVideo' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendVideo' hLogger token chatId video cap = buildTelegramPostRequest' hLogger token "sendVideo" (TelegramSendVideo chatId video cap) []

sendSticker' :: Handle -> String -> Int -> String -> IO (Either T.Text Int)
sendSticker' hLogger token chatId sticker = buildTelegramPostRequest' hLogger token "sendSticker" (TelegramSendSticker chatId sticker) []

sendVideoNote' :: Handle -> String -> Int -> String -> IO (Either T.Text Int)
sendVideoNote' hLogger token chatId video = buildTelegramPostRequest' hLogger token "sendVideoNote" (TelegramSendVideoNote chatId video) []

sendVoice' :: Handle -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
sendVoice' hLogger token chatId voice cap = buildTelegramPostRequest' hLogger token "sendVoice" (TelegramSendVoice chatId voice cap) []

sendContact' :: Handle -> String
    -> Int
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> IO (Either T.Text Int)
sendContact' hLogger token chatId phoneNum fname lname vcard = buildTelegramPostRequest' hLogger token "sendContact" (TelegramSendContact chatId phoneNum fname lname vcard) []

sendLocation' :: Handle -> String
    -> Int
    -> Double
    -> Double
    -> Maybe Double
    -> Maybe Int
    -> Maybe Int
    -> Maybe Int
    -> IO (Either T.Text Int)
sendLocation' hLogger token chatId lat long horac lp hea par = buildTelegramPostRequest' hLogger token "sendLocation" (TelegramSendLocation chatId lat long horac lp hea par) []

sendVenue' :: Handle -> String
    -> Int
    -> Double
    -> Double
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> IO (Either T.Text Int)
sendVenue' hLogger token chatId lat long title address fsid fstype gpid gptype = buildTelegramPostRequest' hLogger token "sendVenue" (TelegramSendVenue chatId lat long title address fsid fstype gpid gptype) []


sendKeyboard' :: Handle -> String -> Int -> IO (Either T.Text Int)
sendKeyboard' hLogger token chatId = buildTelegramPostRequest' hLogger token "sendMessage" (TelegramSendMessage chatId "Choose number reapiting" Nothing (Just keyboard)) []



repeatSendMessage' :: Handle -> Int -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO (Either T.Text Int)
repeatSendMessage' hLogger n token chatId text entities | n > 0 = do
                                                        status <- sendMessage' hLogger token chatId text entities
                                                        case status of
                                                            Left txt -> do
                                                                logError hLogger "Message not send"
                                                                return $ Left "Message not send"
                                                            Right i -> repeatSendMessage' hLogger (n-1) token chatId text entities
                                                      
                                                        --repeatSendMessage (n-1) token chatId text entities
                                                        | otherwise = do
                                                            logDebug hLogger "All messages sended"
                                                            return $ Right 0

repeatSendAnimation' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
repeatSendAnimation' hLogger n token chatId anim cap | n > 0 = do
                                                        status <- sendAnimation' hLogger token chatId anim cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Animation not send"
                                                                        return $ Left "Animation not send"
                                                            Right i -> repeatSendAnimation' hLogger (n-1) token chatId anim cap
                                                        
                                             | otherwise = do
                                                            logDebug hLogger "All Animations sended"
                                                            return $ Right 0
repeatSendAudio' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
repeatSendAudio' hLogger n token chatId audio cap | n > 0 = do
                                                        status <- sendAudio' hLogger token chatId audio cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Audio not send"
                                                                        return $ Left "Audio not send"
                                                            Right i -> repeatSendAudio' hLogger (n-1) token chatId audio cap
                                          | otherwise = do
                                                            logDebug hLogger "All audios sended"
                                                            return $ Right 0
repeatSendDocument' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int) 
repeatSendDocument' hLogger n token chatId doc cap | n > 0 = do
                                                        status <- sendDocument' hLogger token chatId doc cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Document not send"
                                                                        return $ Left "Document not send"
                                                            Right i -> repeatSendDocument' hLogger (n-1) token chatId doc cap
                                          | otherwise = do
                                                            logDebug hLogger "All Documents sended"
                                                            return $ Right 0

repeatSendPhoto' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
repeatSendPhoto' hLogger n token chatId photo cap | n > 0 = do
                                                        status <- sendPhoto' hLogger token chatId photo cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Photo not send"
                                                                        return $ Left "Photo not send"
                                                            Right i -> repeatSendPhoto' hLogger (n-1) token chatId photo cap
                                         | otherwise = do
                                                            logDebug hLogger "All Photo sended"
                                                            return $ Right 0

repeatSendVideo' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
repeatSendVideo' hLogger n token chatId video cap | n > 0 = do
                                                        status <- sendVideo' hLogger token chatId video cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Video not send"
                                                                        return $ Left "Video not send"
                                                            Right i -> repeatSendVideo' hLogger (n-1) token chatId video cap
                                         | otherwise = do
                                                            logDebug hLogger "All Video sended"
                                                            return $ Right 0

repeatSendSticker' :: Handle -> Int -> String -> Int -> String -> IO (Either T.Text Int)
repeatSendSticker' hLogger n token chatId sticker | n > 0 = do
                                                        status <- sendSticker' hLogger token chatId sticker
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Sticker not send"
                                                                        return $ Left "Sticker not send"
                                                            Right i -> repeatSendSticker' hLogger (n-1) token chatId sticker
                                         | otherwise = do
                                                            logDebug hLogger "All stickers sended"
                                                            return $ Right 0

repeatSendVideoNote' :: Handle -> Int -> String -> Int -> String -> IO (Either T.Text Int)
repeatSendVideoNote' hLogger n token chatId videonote | n > 0 = do
                                                        status <- sendVideoNote' hLogger token chatId videonote
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "VideoNote not send"
                                                                        return $ Left "VideoNote not send"
                                                            Right i -> repeatSendVideoNote' hLogger (n-1) token chatId videonote
                                             | otherwise = do
                                                            logDebug hLogger "All VideoNotes sended"
                                                            return $ Right 0

repeatSendVoice' :: Handle -> Int -> String -> Int -> String -> Maybe String -> IO (Either T.Text Int)
repeatSendVoice' hLogger n token chatId voice cap | n > 0 = do
                                                        status <- sendVoice' hLogger token chatId voice cap
                                                        case status of
                                                            Left txt -> do
                                                                        logError hLogger "Voice not send"
                                                                        return $ Left "Voice not send"
                                                            Right i -> repeatSendVoice' hLogger (n-1) token chatId voice cap
                                         | otherwise = do
                                                            logDebug hLogger "All Voices sended"
                                                            return $ Right 0

repeatSendContact' :: Handle -> Int -> String -> Int -> String -> String -> Maybe String -> Maybe String -> IO (Either T.Text Int)
repeatSendContact' hLogger n token chatId phoneNum fname lname vcard | n > 0 = do
                                                                status <- sendContact' hLogger token chatId phoneNum fname lname vcard
                                                                case status of
                                                                    Left txt -> do
                                                                        logError hLogger "Contact not send"
                                                                        return $ Left "Contact not send"
                                                                    Right i -> repeatSendContact' hLogger (n-1) token chatId phoneNum fname lname vcard
                                                            | otherwise = do
                                                                    logDebug hLogger "All Contacts sended"
                                                                    return $ Right 0

repeatSendLocation' :: Handle -> Int -> String -> Int -> Double -> Double -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Int -> IO (Either T.Text Int)
repeatSendLocation' hLogger n token chatId lat long horac lp hea par   | n > 0 = do
                                                                status <- sendLocation' hLogger token chatId lat long horac lp hea par
                                                                case status of
                                                                    Left txt -> do
                                                                        logError hLogger "Location not send"
                                                                        return $ Left "Location not send"
                                                                    Right i -> repeatSendLocation' hLogger (n-1) token chatId lat long horac lp hea par
                                                              | otherwise = do
                                                                    logDebug hLogger "All Locations sended"
                                                                    return $ Right 0

repeatSendVenue' :: Handle -> Int -> String -> Int -> Double -> Double -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Either T.Text Int)
repeatSendVenue' hLogger n token chatId lat long title address fsid fstype gpid gptype   | n > 0 = do
                                                                                    status <- sendVenue' hLogger token chatId lat long title address fsid fstype gpid gptype
                                                                                    case status of
                                                                                        Left txt -> do
                                                                                                logError hLogger "Venue not send"
                                                                                                return $ Left "Venue not send"
                                                                                        Right i -> repeatSendVenue' hLogger (n-1) token chatId lat long title address fsid fstype gpid gptype
                                                                                | otherwise = do
                                                                                        logDebug hLogger "All Venues sended"
                                                                                        return $ Right 0






