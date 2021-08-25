
{-# LANGUAGE OverloadedStrings #-}

module TelegramApi where

import Config (ConfigModules(help, token))
import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class ()
import Data.Aeson (FromJSON(parseJSON), ToJSON, Value)
import Data.Aeson.Types (parseEither)
--import qualified Data.ByteString as B
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Logger (Handle, logDebug, logError, logInfo)
import Network.HTTP.Req
    ( GET(GET)
    , HttpException
    , JsonResponse
    , MonadHttp(handleHttpException)
    , NoReqBody(NoReqBody)
    , POST(POST)
    , QueryParam
    , ReqBodyJson(ReqBodyJson)
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
import TelegramResponses
    ( TelegramAnimation(telegramAnimationFileId)
    , TelegramAudio(telegramAudioFileId)
    , TelegramCallbackQuery(TelegramCallbackQuery)
    , TelegramChat(telegramChatId)
    , TelegramContact(telegramContactFirstName, telegramContactLastName,
                telegramContactPhoneNumber, telegramContactVcard)
    , TelegramDocument(telegramDocumentFileId)
    , TelegramInlineKeyboardButton(TelegramInlineKeyboardButton)
    , TelegramInlineKeyboardMarkup(TelegramInlineKeyboardMarkup)
    , TelegramLocation(telegramLocationHeading,
                 telegramLocationHorizontalAccuracy, telegramLocationLatitude,
                 telegramLocationLivePeriod, telegramLocationLongitude,
                 telegramLocationProximityAlertRadius)
    , TelegramMessage(TelegramMessage, telegramMessageCaption,
                telegramMessageChat, telegramMessageEntities)
    , TelegramMessageEntity
    , TelegramPhotoSize(telegramPhotoSizeFileId)
    , TelegramResponse(TelegramResponse)
    , TelegramSendAnimation(TelegramSendAnimation)
    , TelegramSendAudio(TelegramSendAudio)
    , TelegramSendContact(TelegramSendContact)
    , TelegramSendDocument(TelegramSendDocument)
    , TelegramSendLocation(TelegramSendLocation)
    , TelegramSendMessage(TelegramSendMessage)
    , TelegramSendPhoto(TelegramSendPhoto)
    , TelegramSendSticker(TelegramSendSticker)
    , TelegramSendVenue(TelegramSendVenue)
    , TelegramSendVideo(TelegramSendVideo)
    , TelegramSendVideoNote(TelegramSendVideoNote)
    , TelegramSendVoice(TelegramSendVoice)
    , TelegramSticker(telegramStickerFileId)
    , TelegramUpdate(TelegramUpdate, telegramUpdateId)
    , TelegramUser(telegramUserId)
    , TelegramVenue(telegramVenueAddress, telegramVenueFoursquareId,
              telegramVenueFoursquareType, telegramVenueGooglePlaceId,
              telegramVenueGooglePlaceType, telegramVenueLocation,
              telegramVenueTitle)
    , TelegramVideo(telegramVideoFileId)
    , TelegramVideoNote(telegramVideoNoteFileId)
    , TelegramVoice(telegramVoiceFileId)
    )

instance MonadHttp IO where
    handleHttpException = throwIO

type TelegramToken = String

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params

buildTelegramGetRequest ::
       FromJSON a
    => Handle
    -> TelegramToken
    -> String
    -> [(T.Text, T.Text)]
    -> IO (Either T.Text a)
buildTelegramGetRequest hLogger tgtoken url params = do
    response <- responseBody <$> request
    parseResult response
  where
    request =
        req
            GET
            (https "api.telegram.org" /: T.pack ("bot" ++ tgtoken) /: T.pack url)
            NoReqBody
            jsonResponse
            param
    param = buildParams params
    parseResult r =
        case parseEither parseJSON r of
            Right (TelegramResponse True _ (Just result)) -> do
                return $ Right result
            Right (TelegramResponse False (Just _) _) -> do
                logError hLogger "No response"
                return $ Left "No response"
            Right (TelegramResponse True _ Nothing) -> do
                logError hLogger "No result"
                return $ Left "No result"
            _ -> do
                logError hLogger "Unexpected errorr"
                return $ Left "Unexpected errorr"

getUpdates ::
       FromJSON a
    => Handle
    -> TelegramToken
    -> Maybe Int
    -> IO (Either T.Text a)
getUpdates hLogger tgtoken (Just updId) =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", T.pack $ show updId), ("timeout", "10")]
getUpdates hLogger tgtoken Nothing =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", "0"), ("timeout", "10")]

echo :: Handle -> TelegramToken -> Maybe Int -> String -> [(Int, Int)] -> IO ()
echo hLogger tgtoken updateId help_message listOfUsers = do
    updates <- getUpdates hLogger tgtoken updateId
    b <- answers hLogger help_message tgtoken updates listOfUsers
    let newlistOfUsers = updateListUsers listOfUsers b
    nextUpdateID <- getLastUpdateId hLogger updates
    echo hLogger tgtoken nextUpdateID help_message newlistOfUsers

startTelegramBot' :: Handle -> ConfigModules -> IO ()
startTelegramBot' hLogger botConf =
    catch
        (do logInfo hLogger "Bot Start"
            echo
                hLogger
                (Config.token botConf)
                (Just 0)
                (Config.help botConf)
                []) $ \e -> do
        let err = e :: HttpException -- подумать
        logError hLogger "Bad token"

getLastUpdateId :: Handle -> Either T.Text [TelegramUpdate] -> IO (Maybe Int)
getLastUpdateId hLogger updates =
    case updates of
        Left _ -> do
            return Nothing
        Right [] -> do
            logInfo hLogger "No updates"
            return Nothing
        Right xs -> return $ Just $ (+ 1) $ telegramUpdateId $ last xs

answers ::
       Handle
    -> String
    -> TelegramToken
    -> Either T.Text [TelegramUpdate]
    -> [(Int, Int)]
    -> IO [Maybe (Int, Int)]
answers hLogger help_message tgtoken (Right upd) list =
    mapM (answer' hLogger help_message tgtoken list) upd
answers hLogger _ _ _ _ = do
    logError hLogger "Something wrong"
    return [Nothing]

updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int, Int)]
updateListUsers xs (u:us) = updateListUsers newList us
  where
    newList =
        case u of
            Nothing -> xs
            Just (cid, n) -> newlist' ++ [(cid, n)]
                where newlist' = filter ((/= cid) . fst) xs
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
buttons = [[TelegramInlineKeyboardButton x x] | x <- ["1", "2", "3", "4", "5"]]

keyboard :: TelegramInlineKeyboardMarkup
keyboard = TelegramInlineKeyboardMarkup buttons

answer' ::
       Handle
    -> String
    -> TelegramToken
    -> [(Int, Int)]
    -> TelegramUpdate
    -> IO (Maybe (Int, Int))
answer' hLogger _ tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    --n <- findRepeatNumber list chatId
    status <- sendKeyboard' hLogger tgtoken chatId
    case status of
        Left _ -> logError hLogger "Keyboard not send"
        Right _ -> logDebug hLogger "Keyboard sended"
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger help_message tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    status <- sendMessage' hLogger tgtoken chatId help_message Nothing
    case status of
        Left _ -> logError hLogger "Help message not sended"
        Right _ -> logDebug hLogger "Help message sended"
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just text) _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendMessage' hLogger n tgtoken chatId ansText entities
    return Nothing
  where
    ansText = text
    entities = telegramMessageEntities message
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ (Just anim) _ _ _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendAnimation' hLogger n tgtoken chatId animid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    animid = telegramAnimationFileId anim
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just audio) _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendAudio' hLogger n tgtoken chatId audioid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    audioid = telegramAudioFileId audio
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ Nothing _ (Just doc) _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendDocument' hLogger n tgtoken chatId docid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    docid = telegramDocumentFileId doc
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (photo:_)) _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendPhoto' hLogger n tgtoken chatId photoid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    photoid = telegramPhotoSizeFileId photo
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just video) _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVideo' hLogger n tgtoken chatId videoid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    videoid = telegramVideoFileId video
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just sticker) _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendSticker' hLogger n tgtoken chatId stickerid
    return Nothing
  where
    stickerid = telegramStickerFileId sticker
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just videoNote) _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVideoNote' hLogger n tgtoken chatId videoid
    return Nothing
  where
    videoid = telegramVideoNoteFileId videoNote
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just voice) _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVoice' hLogger n tgtoken chatId voiceid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    voiceid = telegramVoiceFileId voice
    chatId = telegramChatId $ telegramMessageChat message
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just contact) _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendContact' hLogger n tgtoken chatId phoneNumber fname lname vcard
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
    phoneNumber = telegramContactPhoneNumber contact
    fname = telegramContactFirstName contact
    lname = telegramContactLastName contact
    vcard = telegramContactVcard contact
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just location) Nothing)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendLocation' hLogger n tgtoken chatId lat long horac lp hea par
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
    lat = telegramLocationLatitude location
    long = telegramLocationLongitude location
    horac = telegramLocationHorizontalAccuracy location
    lp = telegramLocationLivePeriod location
    hea = telegramLocationHeading location
    par = telegramLocationProximityAlertRadius location
answer' hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just venue))) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVenue'
        hLogger
        n
        tgtoken
        chatId
        lat
        long
        title
        address
        fsid
        fstype
        gpid
        gptype
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
    lat = telegramLocationLatitude $ telegramVenueLocation venue
    long = telegramLocationLongitude $ telegramVenueLocation venue
    title = telegramVenueTitle venue
    address = telegramVenueAddress venue
    fsid = telegramVenueFoursquareId venue
    fstype = telegramVenueFoursquareType venue
    gpid = telegramVenueGooglePlaceId venue
    gptype = telegramVenueGooglePlaceType venue
answer' hLogger _ tgtoken _ (TelegramUpdate _ _ (Just (TelegramCallbackQuery _ user (Just _) _ (Just dat)))) = do
--answer' hLogger _ tgtoken list (TelegramUpdate _ _ (Just callback@(TelegramCallbackQuery _ user (Just message) _ (Just dat)))) = do
    status <- sendMessage' hLogger tgtoken chatId text Nothing
    case status of
        Left _ -> do
            logError hLogger "Keyboard not sended"
            return Nothing
        Right _ -> do
            logDebug hLogger "Keyboard sended"
            return $ Just (chatId, read dat :: Int)
  where
    chatId = telegramUserId user
    text = "Number of reapeting " ++ dat
answer' _ _ _ _ _ = return Nothing

buildTelegramPostRequest' ::
       ToJSON b
    => Handle
    -> String
    -> String
    -> b
    -> [(T.Text, T.Text)]
    -> IO (Either T.Text Int)
buildTelegramPostRequest' _ tgtoken url body params =
--buildTelegramPostRequest' hLogger tgtoken url body params =
    runReq defaultHttpConfig $ do
        r <-
            req
                POST
                (https "api.telegram.org" /: T.pack ("bot" ++ tgtoken) /:
                 T.pack url)
                (ReqBodyJson body)
                jsonResponse
                param
        if responseStatusCode (r :: JsonResponse Value) == 200
            then return $ Right 200
            else return $ Left "No response"
  where
    param = buildParams params

sendMessage' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe [TelegramMessageEntity]
    -> IO (Either T.Text Int)
sendMessage' hLogger tgtoken chatId text ent =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage chatId text ent Nothing)
        []

sendAnimation' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendAnimation' hLogger tgtoken chatId anim cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendAnimation"
        (TelegramSendAnimation chatId anim cap)
        []

sendAudio' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendAudio' hLogger tgtoken chatId audio cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendAudio"
        (TelegramSendAudio chatId audio cap)
        []

sendDocument' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendDocument' hLogger tgtoken chatId doc cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendDocument"
        (TelegramSendDocument chatId doc cap)
        []

sendPhoto' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendPhoto' hLogger tgtoken chatId photo cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendPhoto"
        (TelegramSendPhoto chatId photo cap)
        []

sendVideo' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendVideo' hLogger tgtoken chatId video cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendVideo"
        (TelegramSendVideo chatId video cap)
        []

sendSticker' :: Handle -> String -> Int -> String -> IO (Either T.Text Int)
sendSticker' hLogger tgtoken chatId sticker =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendSticker"
        (TelegramSendSticker chatId sticker)
        []

sendVideoNote' :: Handle -> String -> Int -> String -> IO (Either T.Text Int)
sendVideoNote' hLogger tgtoken chatId video =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendVideoNote"
        (TelegramSendVideoNote chatId video)
        []

sendVoice' ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
sendVoice' hLogger tgtoken chatId voice cap =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendVoice"
        (TelegramSendVoice chatId voice cap)
        []

sendContact' ::
       Handle
    -> String
    -> Int
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> IO (Either T.Text Int)
sendContact' hLogger tgtoken chatId phoneNum fname lname vcard =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendContact"
        (TelegramSendContact chatId phoneNum fname lname vcard)
        []

sendLocation' ::
       Handle
    -> String
    -> Int
    -> Double
    -> Double
    -> Maybe Double
    -> Maybe Int
    -> Maybe Int
    -> Maybe Int
    -> IO (Either T.Text Int)
sendLocation' hLogger tgtoken chatId lat long horac lp hea par =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendLocation"
        (TelegramSendLocation chatId lat long horac lp hea par)
        []

sendVenue' ::
       Handle
    -> String
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
sendVenue' hLogger tgtoken chatId lat long title address fsid fstype gpid gptype =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendVenue"
        (TelegramSendVenue chatId lat long title address fsid fstype gpid gptype)
        []

sendKeyboard' :: Handle -> String -> Int -> IO (Either T.Text Int)
sendKeyboard' hLogger tgtoken chatId =
    buildTelegramPostRequest'
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage
             chatId
             "Choose number reapiting"
             Nothing
             (Just keyboard))
        []

repeatSendMessage' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe [TelegramMessageEntity]
    -> IO (Either T.Text Int)
repeatSendMessage' hLogger n tgtoken chatId text entities
    | n > 0 = do
        status <- sendMessage' hLogger tgtoken chatId text entities
        case status of
            Left _ -> do
                logError hLogger "Message not send"
                return $ Left "Message not send"
            Right _ ->
                repeatSendMessage' hLogger (n - 1) tgtoken chatId text entities
                                                        
    | otherwise = do
        logDebug hLogger "All messages sended"
        return $ Right 0

repeatSendAnimation' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendAnimation' hLogger n tgtoken chatId anim cap
    | n > 0 = do
        status <- sendAnimation' hLogger tgtoken chatId anim cap
        case status of
            Left _ -> do
                logError hLogger "Animation not send"
                return $ Left "Animation not send"
            Right _ ->
                repeatSendAnimation' hLogger (n - 1) tgtoken chatId anim cap
    | otherwise = do
        logDebug hLogger "All Animations sended"
        return $ Right 0

repeatSendAudio' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendAudio' hLogger n tgtoken chatId audio cap
    | n > 0 = do
        status <- sendAudio' hLogger tgtoken chatId audio cap
        case status of
            Left _ -> do
                logError hLogger "Audio not send"
                return $ Left "Audio not send"
            Right _ -> repeatSendAudio' hLogger (n - 1) tgtoken chatId audio cap
    | otherwise = do
        logDebug hLogger "All audios sended"
        return $ Right 0

repeatSendDocument' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendDocument' hLogger n tgtoken chatId doc cap
    | n > 0 = do
        status <- sendDocument' hLogger tgtoken chatId doc cap
        case status of
            Left _ -> do
                logError hLogger "Document not send"
                return $ Left "Document not send"
            Right _ -> repeatSendDocument' hLogger (n - 1) tgtoken chatId doc cap
    | otherwise = do
        logDebug hLogger "All Documents sended"
        return $ Right 0

repeatSendPhoto' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendPhoto' hLogger n tgtoken chatId photo cap
    | n > 0 = do
        status <- sendPhoto' hLogger tgtoken chatId photo cap
        case status of
            Left _ -> do
                logError hLogger "Photo not send"
                return $ Left "Photo not send"
            Right _ -> repeatSendPhoto' hLogger (n - 1) tgtoken chatId photo cap
    | otherwise = do
        logDebug hLogger "All Photo sended"
        return $ Right 0

repeatSendVideo' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendVideo' hLogger n tgtoken chatId video cap
    | n > 0 = do
        status <- sendVideo' hLogger tgtoken chatId video cap
        case status of
            Left _ -> do
                logError hLogger "Video not send"
                return $ Left "Video not send"
            Right _ -> repeatSendVideo' hLogger (n - 1) tgtoken chatId video cap
    | otherwise = do
        logDebug hLogger "All Video sended"
        return $ Right 0

repeatSendSticker' ::
       Handle -> Int -> String -> Int -> String -> IO (Either T.Text Int)
repeatSendSticker' hLogger n tgtoken chatId sticker
    | n > 0 = do
        status <- sendSticker' hLogger tgtoken chatId sticker
        case status of
            Left _ -> do
                logError hLogger "Sticker not send"
                return $ Left "Sticker not send"
            Right _ -> repeatSendSticker' hLogger (n - 1) tgtoken chatId sticker
    | otherwise = do
        logDebug hLogger "All stickers sended"
        return $ Right 0

repeatSendVideoNote' ::
       Handle -> Int -> String -> Int -> String -> IO (Either T.Text Int)
repeatSendVideoNote' hLogger n tgtoken chatId videonote
    | n > 0 = do
        status <- sendVideoNote' hLogger tgtoken chatId videonote
        case status of
            Left _ -> do
                logError hLogger "VideoNote not send"
                return $ Left "VideoNote not send"
            Right _ ->
                repeatSendVideoNote' hLogger (n - 1) tgtoken chatId videonote
    | otherwise = do
        logDebug hLogger "All VideoNotes sended"
        return $ Right 0

repeatSendVoice' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendVoice' hLogger n tgtoken chatId voice cap
    | n > 0 = do
        status <- sendVoice' hLogger tgtoken chatId voice cap
        case status of
            Left _ -> do
                logError hLogger "Voice not send"
                return $ Left "Voice not send"
            Right _ -> repeatSendVoice' hLogger (n - 1) tgtoken chatId voice cap
    | otherwise = do
        logDebug hLogger "All Voices sended"
        return $ Right 0

repeatSendContact' ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> IO (Either T.Text Int)
repeatSendContact' hLogger n tgtoken chatId phoneNum fname lname vcard
    | n > 0 = do
        status <- sendContact' hLogger tgtoken chatId phoneNum fname lname vcard
        case status of
            Left _ -> do
                logError hLogger "Contact not send"
                return $ Left "Contact not send"
            Right _ ->
                repeatSendContact'
                    hLogger
                    (n - 1)
                    tgtoken
                    chatId
                    phoneNum
                    fname
                    lname
                    vcard
    | otherwise = do
        logDebug hLogger "All Contacts sended"
        return $ Right 0

repeatSendLocation' ::
       Handle
    -> Int
    -> String
    -> Int
    -> Double
    -> Double
    -> Maybe Double
    -> Maybe Int
    -> Maybe Int
    -> Maybe Int
    -> IO (Either T.Text Int)
repeatSendLocation' hLogger n tgtoken chatId lat long horac lp hea par
    | n > 0 = do
        status <- sendLocation' hLogger tgtoken chatId lat long horac lp hea par
        case status of
            Left _ -> do
                logError hLogger "Location not send"
                return $ Left "Location not send"
            Right _ ->
                repeatSendLocation'
                    hLogger
                    (n - 1)
                    tgtoken
                    chatId
                    lat
                    long
                    horac
                    lp
                    hea
                    par
    | otherwise = do
        logDebug hLogger "All Locations sended"
        return $ Right 0

repeatSendVenue' ::
       Handle
    -> Int
    -> String
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
repeatSendVenue' hLogger n tgtoken chatId lat long title address fsid fstype gpid gptype
    | n > 0 = do
        status <-
            sendVenue'
                hLogger
                tgtoken
                chatId
                lat
                long
                title
                address
                fsid
                fstype
                gpid
                gptype
        case status of
            Left _ -> do
                logError hLogger "Venue not send"
                return $ Left "Venue not send"
            Right _ ->
                repeatSendVenue'
                    hLogger
                    (n - 1)
                    tgtoken
                    chatId
                    lat
                    long
                    title
                    address
                    fsid
                    fstype
                    gpid
                    gptype
    | otherwise = do
        logDebug hLogger "All Venues sended"
        return $ Right 0






