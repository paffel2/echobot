{-# LANGUAGE OverloadedStrings #-}

module Telegram.API where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Logger (Handle, logInfo)
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
    , TelegramContact(telegramContactFirstName, telegramContactLastName,
                telegramContactPhoneNumber, telegramContactVcard)
    , TelegramDocument(telegramDocumentFileId)
    , TelegramLocation(telegramLocationHeading,
                 telegramLocationHorizontalAccuracy, telegramLocationLatitude,
                 telegramLocationLivePeriod, telegramLocationLongitude,
                 telegramLocationProximityAlertRadius)
    , TelegramMessageEntity
    , TelegramPhotoSize(telegramPhotoSizeFileId)
    , TelegramSticker(telegramStickerFileId)
    , TelegramText
    , TelegramUpdate(telegramUpdateId)
    , TelegramUser
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

updateListUsers :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
updateListUsers xs ((cid,n):us) = updateListUsers newList us
  where
    newList =
        newlist' ++ [(cid, n)]
    newlist' = filter ((/= cid) . fst) xs
updateListUsers xs [] = xs

findRepeatNumber :: [(Int, Int)] -> Int -> IO Int
findRepeatNumber listOfUsers chatId = do
    let n = lookup chatId listOfUsers
    case n of
        Just x -> do
            return x
        Nothing -> do
            return 1

sendTextMessage ::
       Handle
    -> String
    -> Int
    -> TelegramText
    -> Maybe [TelegramMessageEntity]
    -> IO (Maybe Int)
sendTextMessage hLogger tgtoken chatId text ent =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage chatId text ent Nothing)
        []

sendAnimationMessage ::
       Handle
    -> String
    -> Int
    -> TelegramAnimation
    -> Maybe String
    -> IO (Maybe Int)
sendAnimationMessage hLogger tgtoken chatId anim cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendAnimation"
        (TelegramSendAnimation chatId animId cap)
        []
  where
    animId = telegramAnimationFileId anim

sendAudioMessage ::
       Handle
    -> String
    -> Int
    -> TelegramAudio
    -> Maybe String
    -> IO (Maybe Int)
sendAudioMessage hLogger tgtoken chatId audio cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendAudio"
        (TelegramSendAudio chatId audioId cap)
        []
  where
    audioId = telegramAudioFileId audio

sendDocumentMessage ::
       Handle
    -> String
    -> Int
    -> TelegramDocument
    -> Maybe String
    -> IO (Maybe Int)
sendDocumentMessage hLogger tgtoken chatId doc cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendDocument"
        (TelegramSendDocument chatId docId cap)
        []
  where
    docId = telegramDocumentFileId doc

sendPhotoMessage ::
       Handle
    -> String
    -> Int
    -> [TelegramPhotoSize]
    -> Maybe String
    -> IO (Maybe Int)
sendPhotoMessage hLogger tgtoken chatId (photo:_) cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendPhoto"
        (TelegramSendPhoto chatId photoId cap)
        []
  where
    photoId = telegramPhotoSizeFileId photo
sendPhotoMessage _ _ _ _ _ = return Nothing

sendVideoMessage ::
       Handle
    -> String
    -> Int
    -> TelegramVideo
    -> Maybe String
    -> IO (Maybe Int)
sendVideoMessage hLogger tgtoken chatId video cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVideo"
        (TelegramSendVideo chatId videoId cap)
        []
  where
    videoId = telegramVideoFileId video

sendStickerMessage ::
       Handle -> String -> Int -> TelegramSticker -> IO (Maybe Int)
sendStickerMessage hLogger tgtoken chatId sticker =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendSticker"
        (TelegramSendSticker chatId stickerId)
        []
  where
    stickerId = telegramStickerFileId sticker

sendVideoNoteMessage ::
       Handle -> String -> Int -> TelegramVideoNote -> IO (Maybe Int)
sendVideoNoteMessage hLogger tgtoken chatId videoNote =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVideoNote"
        (TelegramSendVideoNote chatId videoNoteId)
        []
  where
    videoNoteId = telegramVideoNoteFileId videoNote

sendVoiceMessage ::
       Handle
    -> String
    -> Int
    -> TelegramVoice
    -> Maybe String
    -> IO (Maybe Int)
sendVoiceMessage hLogger tgtoken chatId voice cap =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVoice"
        (TelegramSendVoice chatId voiceId cap)
        []
  where
    voiceId = telegramVoiceFileId voice

sendContactMessage ::
       Handle -> String -> Int -> TelegramContact -> IO (Maybe Int)
sendContactMessage hLogger tgtoken chatId contact =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendContact"
        (TelegramSendContact chatId phoneNum fname lname vcard)
        []
  where
    phoneNum = telegramContactPhoneNumber contact
    fname = telegramContactFirstName contact
    lname = telegramContactLastName contact
    vcard = telegramContactVcard contact

sendLocationMessage ::
       Handle -> String -> Int -> TelegramLocation -> IO (Maybe Int)
sendLocationMessage hLogger tgtoken chatId location =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendLocation"
        (TelegramSendLocation chatId lat long horac lp hea par)
        []
  where
    lat = telegramLocationLatitude location
    long = telegramLocationLongitude location
    horac = telegramLocationHorizontalAccuracy location
    lp = telegramLocationLivePeriod location
    hea = telegramLocationHeading location
    par = telegramLocationProximityAlertRadius location

sendVenueMessage :: Handle -> String -> Int -> TelegramVenue -> IO (Maybe Int)
sendVenueMessage hLogger tgtoken chatId venue =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendVenue"
        (TelegramSendVenue chatId lat long title address fsid fstype gpid gptype)
        []
  where
    lat = telegramLocationLatitude $ telegramVenueLocation venue
    long = telegramLocationLongitude $ telegramVenueLocation venue
    title = telegramVenueTitle venue
    address = telegramVenueAddress venue
    fsid = telegramVenueFoursquareId venue
    fstype = telegramVenueFoursquareType venue
    gpid = telegramVenueGooglePlaceId venue
    gptype = telegramVenueGooglePlaceType venue

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
