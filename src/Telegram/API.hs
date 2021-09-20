{-# LANGUAGE OverloadedStrings #-}

module Telegram.API where

import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Logger (Handle, logInfo)
import Telegram.BuildRequest
    ( 
     buildTelegramGetRequest
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
import Telegram.Types
    ( Caption, ChatId, StatusResult, TelegramToken, UpdateId ) 


getMe :: Handle IO-> TelegramToken -> IO (Maybe TelegramUser)
getMe hLogger tgtoken = buildTelegramGetRequest hLogger tgtoken "getMe" []

getUpdates :: FromJSON a => Handle IO-> TelegramToken -> Maybe UpdateId -> IO (Maybe a)
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

getLastUpdateId :: Handle IO-> Maybe [TelegramUpdate] -> IO (Maybe UpdateId)
getLastUpdateId hLogger updates =
    case updates of
        Nothing -> do
            return Nothing
        Just [] -> do
            logInfo hLogger "No updates"
            return Nothing
        Just xs -> return $ Just $ (+ 1) $ telegramUpdateId $ last xs

{-updateListUsers :: RepeatsList -> [Maybe Repeats] -> RepeatsList
updateListUsers xs (u:us) = updateListUsers newList us
  where
    newList =
        case u of
            Nothing -> xs
            Just (Repeats cid n) -> newList' ++ [Repeats cid n]
                where --newlist' = filter ((/= cid) . fst) xs
                    newList' = filter ((/= cid) . chat_id) xs
updateListUsers xs [] = xs

findRepeatNumber :: RepeatsList -> ChatId -> IO RepeatsNum
findRepeatNumber listOfUsers chatId = do
    --let n = lookup chatId listOfUsers
    let rep = find (\x -> chatId == chat_id x ) listOfUsers
    case rep of
      Nothing -> return 1
      Just re -> return $ repeats_num re
        {-Just x -> do
            return x
        Nothing -> do
            return 1-}-}

sendTextMessage ::
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramText
    -> Maybe [TelegramMessageEntity]
    -> IO (Maybe StatusResult)
sendTextMessage hLogger tgtoken chatId text ent =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage chatId text ent Nothing)
        []

sendAnimationMessage ::
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramAnimation
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramAudio
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramDocument
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> [TelegramPhotoSize]
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramVideo
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO-> TelegramToken -> ChatId -> TelegramSticker -> IO (Maybe StatusResult)
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
       Handle IO-> TelegramToken -> ChatId -> TelegramVideoNote -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramVoice
    -> Maybe Caption
    -> IO (Maybe StatusResult)
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
       Handle IO-> TelegramToken -> ChatId -> TelegramContact -> IO (Maybe StatusResult)
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
       Handle IO-> TelegramToken -> ChatId -> TelegramLocation -> IO (Maybe StatusResult)
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

sendVenueMessage :: Handle IO -> TelegramToken -> ChatId -> TelegramVenue -> IO (Maybe StatusResult)
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

sendKeyboard :: Handle IO -> TelegramToken -> ChatId -> IO (Maybe StatusResult)
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
       Handle IO
    -> TelegramToken
    -> ChatId
    -> TelegramText
    -> Maybe [TelegramMessageEntity]
    -> IO (Maybe StatusResult)
sendMessage hLogger tgtoken chatId text ent =
    buildTelegramPostRequest
        hLogger
        tgtoken
        "sendMessage"
        (TelegramSendMessage chatId text ent Nothing)
        []
