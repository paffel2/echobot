{-# LANGUAGE OverloadedStrings #-}

module Telegram.API where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Logger (Handle, logDebug, logError, logInfo)
import Telegram.BuildRequest
    ( TelegramToken
    , buildTelegramGetRequest
    , buildTelegramPostRequest
    )
import Telegram.Keyboard (keyboard)
import Telegram.Requests
    ( TelegramSendAnimation(TelegramSendAnimation)
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
    )
import Telegram.Responses
    ( TelegramAnimation(telegramAnimationFileId)
    , TelegramAudio(telegramAudioFileId)
    , TelegramCallbackQuery(TelegramCallbackQuery)
    , TelegramChat(telegramChatId)
    , TelegramContact(telegramContactFirstName, telegramContactLastName,
                telegramContactPhoneNumber, telegramContactVcard)
    , TelegramDocument(telegramDocumentFileId)
    , TelegramLocation(telegramLocationHeading,
                 telegramLocationHorizontalAccuracy, telegramLocationLatitude,
                 telegramLocationLivePeriod, telegramLocationLongitude,
                 telegramLocationProximityAlertRadius)
    , TelegramMessage(TelegramMessage, telegramMessageCaption,
                telegramMessageChat, telegramMessageEntities)
    , TelegramMessageEntity
    , TelegramPhotoSize(telegramPhotoSizeFileId)
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

getMe :: Handle -> TelegramToken -> IO (Maybe TelegramUser)
getMe hLogger tgtoken = buildTelegramGetRequest hLogger tgtoken "getMe" []

getUpdates :: FromJSON a => Handle -> TelegramToken -> Maybe Int -> IO (Maybe a)
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

getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe Int)
getLastUpdateId hLogger updates =
    case updates of
        Nothing -> do
            return Nothing
        Just [] -> do
            logInfo hLogger "No updates"
            return Nothing
        Just xs -> return $ Just $ (+ 1) $ telegramUpdateId $ last xs

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

answers ::
       Handle
    -> String
    -> TelegramToken
    -> Maybe [TelegramUpdate]
    -> [(Int, Int)]
    -> IO [Maybe (Int, Int)]
answers hLogger help_message tgtoken (Just upd) list =
    mapM (answer hLogger help_message tgtoken list) upd
answers hLogger _ _ _ _ = do
    logError hLogger "Something wrong"
    return [Nothing]

answer ::
       Handle
    -> String
    -> TelegramToken
    -> [(Int, Int)]
    -> TelegramUpdate
    -> IO (Maybe (Int, Int))
answer hLogger _ tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    status <- sendKeyboard hLogger tgtoken chatId
    case status of
        Nothing -> logError hLogger "Keyboard not send"
        Just _ -> logDebug hLogger "Keyboard sended"
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger help_message tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    status <- sendMessage hLogger tgtoken chatId help_message Nothing
    case status of
        Nothing -> logError hLogger "Help message not sended"
        Just _ -> logDebug hLogger "Help message sended"
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just text) _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendMessage hLogger n tgtoken chatId ansText entities
    return Nothing
  where
    ansText = text
    entities = telegramMessageEntities message
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ (Just anim) _ _ _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendAnimation hLogger n tgtoken chatId animid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    animid = telegramAnimationFileId anim
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just audio) _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendAudio hLogger n tgtoken chatId audioid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    audioid = telegramAudioFileId audio
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ Nothing _ (Just doc) _ _ _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendDocument hLogger n tgtoken chatId docid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    docid = telegramDocumentFileId doc
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (photo:_)) _ _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendPhoto hLogger n tgtoken chatId photoid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    photoid = telegramPhotoSizeFileId photo
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just video) _ _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVideo hLogger n tgtoken chatId videoid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    videoid = telegramVideoFileId video
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just sticker) _ _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendSticker hLogger n tgtoken chatId stickerid
    return Nothing
  where
    stickerid = telegramStickerFileId sticker
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just videoNote) _ _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVideoNote hLogger n tgtoken chatId videoid
    return Nothing
  where
    videoid = telegramVideoNoteFileId videoNote
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just voice) _ _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendVoice hLogger n tgtoken chatId voiceid cap
    return Nothing
  where
    cap = telegramMessageCaption message
    voiceid = telegramVoiceFileId voice
    chatId = telegramChatId $ telegramMessageChat message
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just contact) _ _)) _) = do
    n <- findRepeatNumber list chatId
    _ <-
        repeatSendContact hLogger n tgtoken chatId phoneNumber fname lname vcard
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
    phoneNumber = telegramContactPhoneNumber contact
    fname = telegramContactFirstName contact
    lname = telegramContactLastName contact
    vcard = telegramContactVcard contact
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just location) Nothing)) _) = do
    n <- findRepeatNumber list chatId
    _ <- repeatSendLocation hLogger n tgtoken chatId lat long horac lp hea par
    return Nothing
  where
    chatId = telegramChatId $ telegramMessageChat message
    lat = telegramLocationLatitude location
    long = telegramLocationLongitude location
    horac = telegramLocationHorizontalAccuracy location
    lp = telegramLocationLivePeriod location
    hea = telegramLocationHeading location
    par = telegramLocationProximityAlertRadius location
answer hLogger _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just venue))) _) = do
    n <- findRepeatNumber list chatId
    _ <-
        repeatSendVenue
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
answer hLogger _ tgtoken _ (TelegramUpdate _ _ (Just (TelegramCallbackQuery _ user (Just _) _ (Just dat)))) = do
    status <- sendMessage hLogger tgtoken chatId text Nothing
    case status of
        Nothing -> do
            logError hLogger "Keyboard not sended"
            return Nothing
        Just _ -> do
            logDebug hLogger "Keyboard sended"
            return $ Just (chatId, read dat :: Int)
  where
    chatId = telegramUserId user
    text = "Number of reapeting " ++ dat
answer _ _ _ _ _ = return Nothing

sendMessage ::
       Handle
    -> String
    -> Int
    -> String
    -> Maybe [TelegramMessageEntity]
    -> IO (Maybe Int)
sendMessage hLogger tgtoken chatId text ent =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage chatId text ent Nothing)
        []

sendAnimation ::
       Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendAnimation hLogger tgtoken chatId anim cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendAnimation"
        (TelegramSendAnimation chatId anim cap)
        []

sendAudio :: Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendAudio hLogger tgtoken chatId audio cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendAudio"
        (TelegramSendAudio chatId audio cap)
        []

sendDocument ::
       Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendDocument hLogger tgtoken chatId doc cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendDocument"
        (TelegramSendDocument chatId doc cap)
        []

sendPhoto :: Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendPhoto hLogger tgtoken chatId photo cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendPhoto"
        (TelegramSendPhoto chatId photo cap)
        []

sendVideo :: Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendVideo hLogger tgtoken chatId video cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVideo"
        (TelegramSendVideo chatId video cap)
        []

sendSticker :: Handle -> String -> Int -> String -> IO (Maybe Int)
sendSticker hLogger tgtoken chatId sticker =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendSticker"
        (TelegramSendSticker chatId sticker)
        []

sendVideoNote :: Handle -> String -> Int -> String -> IO (Maybe Int)
sendVideoNote hLogger tgtoken chatId video =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVideoNote"
        (TelegramSendVideoNote chatId video)
        []

sendVoice :: Handle -> String -> Int -> String -> Maybe String -> IO (Maybe Int)
sendVoice hLogger tgtoken chatId voice cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVoice"
        (TelegramSendVoice chatId voice cap)
        []

sendContact ::
       Handle
    -> String
    -> Int
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> IO (Maybe Int)
sendContact hLogger tgtoken chatId phoneNum fname lname vcard =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendContact"
        (TelegramSendContact chatId phoneNum fname lname vcard)
        []

sendLocation ::
       Handle
    -> String
    -> Int
    -> Double
    -> Double
    -> Maybe Double
    -> Maybe Int
    -> Maybe Int
    -> Maybe Int
    -> IO (Maybe Int)
sendLocation hLogger tgtoken chatId lat long horac lp hea par =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendLocation"
        (TelegramSendLocation chatId lat long horac lp hea par)
        []

sendVenue ::
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
    -> IO (Maybe Int)
sendVenue hLogger tgtoken chatId lat long title address fsid fstype gpid gptype =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVenue"
        (TelegramSendVenue chatId lat long title address fsid fstype gpid gptype)
        []

sendKeyboard :: Handle -> String -> Int -> IO (Maybe Int)
sendKeyboard hLogger tgtoken chatId =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage
             chatId
             "Choose number reapiting"
             Nothing
             (Just keyboard))
        []

repeatSendMessage ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe [TelegramMessageEntity]
    -> IO (Maybe Int)
repeatSendMessage hLogger n tgtoken chatId text entities
    | n > 0 = do
        status <- sendMessage hLogger tgtoken chatId text entities
        case status of
            Nothing -> do
                logError hLogger "Message not send"
                return Nothing
            Just _ ->
                repeatSendMessage hLogger (n - 1) tgtoken chatId text entities
    | otherwise = do
        logDebug hLogger "All messages sended"
        return $ Just 200

repeatSendAnimation ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendAnimation hLogger n tgtoken chatId anim cap
    | n > 0 = do
        status <- sendAnimation hLogger tgtoken chatId anim cap
        case status of
            Nothing -> do
                logError hLogger "Animation not send"
                return Nothing
            Just _ ->
                repeatSendAnimation hLogger (n - 1) tgtoken chatId anim cap
    | otherwise = do
        logDebug hLogger "All Animations sended"
        return $ Just 200

repeatSendAudio ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendAudio hLogger n tgtoken chatId audio cap
    | n > 0 = do
        status <- sendAudio hLogger tgtoken chatId audio cap
        case status of
            Nothing -> do
                logError hLogger "Audio not send"
                return Nothing
            Just _ -> repeatSendAudio hLogger (n - 1) tgtoken chatId audio cap
    | otherwise = do
        logDebug hLogger "All audios sended"
        return $ Just 200

repeatSendDocument ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendDocument hLogger n tgtoken chatId doc cap
    | n > 0 = do
        status <- sendDocument hLogger tgtoken chatId doc cap
        case status of
            Nothing -> do
                logError hLogger "Document not send"
                return Nothing
            Just _ -> repeatSendDocument hLogger (n - 1) tgtoken chatId doc cap
    | otherwise = do
        logDebug hLogger "All Documents sended"
        return $ Just 200

repeatSendPhoto ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendPhoto hLogger n tgtoken chatId photo cap
    | n > 0 = do
        status <- sendPhoto hLogger tgtoken chatId photo cap
        case status of
            Nothing -> do
                logError hLogger "Photo not send"
                return Nothing
            Just _ -> repeatSendPhoto hLogger (n - 1) tgtoken chatId photo cap
    | otherwise = do
        logDebug hLogger "All Photo sended"
        return $ Just 200

repeatSendVideo ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendVideo hLogger n tgtoken chatId video cap
    | n > 0 = do
        status <- sendVideo hLogger tgtoken chatId video cap
        case status of
            Nothing -> do
                logError hLogger "Video not send"
                return Nothing
            Just _ -> repeatSendVideo hLogger (n - 1) tgtoken chatId video cap
    | otherwise = do
        logDebug hLogger "All Video sended"
        return $ Just 200

repeatSendSticker :: Handle -> Int -> String -> Int -> String -> IO (Maybe Int)
repeatSendSticker hLogger n tgtoken chatId sticker
    | n > 0 = do
        status <- sendSticker hLogger tgtoken chatId sticker
        case status of
            Nothing -> do
                logError hLogger "Sticker not send"
                return Nothing
            Just _ -> repeatSendSticker hLogger (n - 1) tgtoken chatId sticker
    | otherwise = do
        logDebug hLogger "All stickers sended"
        return $ Just 200

repeatSendVideoNote ::
       Handle -> Int -> String -> Int -> String -> IO (Maybe Int)
repeatSendVideoNote hLogger n tgtoken chatId videonote
    | n > 0 = do
        status <- sendVideoNote hLogger tgtoken chatId videonote
        case status of
            Nothing -> do
                logError hLogger "VideoNote not send"
                return Nothing
            Just _ ->
                repeatSendVideoNote hLogger (n - 1) tgtoken chatId videonote
    | otherwise = do
        logDebug hLogger "All VideoNotes sended"
        return $ Just 200

repeatSendVoice ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendVoice hLogger n tgtoken chatId voice cap
    | n > 0 = do
        status <- sendVoice hLogger tgtoken chatId voice cap
        case status of
            Nothing -> do
                logError hLogger "Voice not send"
                return Nothing
            Just _ -> repeatSendVoice hLogger (n - 1) tgtoken chatId voice cap
    | otherwise = do
        logDebug hLogger "All Voices sended"
        return $ Just 200

repeatSendContact ::
       Handle
    -> Int
    -> String
    -> Int
    -> String
    -> String
    -> Maybe String
    -> Maybe String
    -> IO (Maybe Int)
repeatSendContact hLogger n tgtoken chatId phoneNum fname lname vcard
    | n > 0 = do
        status <- sendContact hLogger tgtoken chatId phoneNum fname lname vcard
        case status of
            Nothing -> do
                logError hLogger "Contact not send"
                return Nothing
            Just _ ->
                repeatSendContact
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
        return $ Just 200

repeatSendLocation ::
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
    -> IO (Maybe Int)
repeatSendLocation hLogger n tgtoken chatId lat long horac lp hea par
    | n > 0 = do
        status <- sendLocation hLogger tgtoken chatId lat long horac lp hea par
        case status of
            Nothing -> do
                logError hLogger "Location not send"
                return Nothing
            Just _ ->
                repeatSendLocation
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
        return $ Just 200

repeatSendVenue ::
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
    -> IO (Maybe Int)
repeatSendVenue hLogger n tgtoken chatId lat long title address fsid fstype gpid gptype
    | n > 0 = do
        status <-
            sendVenue
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
            Nothing -> do
                logError hLogger "Venue not send"
                return Nothing
            Just _ ->
                repeatSendVenue
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
        return $ Just 200
