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
    ( TelegramVenue(telegramVenueLocation, telegramVenueTitle,
                    telegramVenueAddress, telegramVenueFoursquareId,
                    telegramVenueFoursquareType, telegramVenueGooglePlaceId,
                    telegramVenueGooglePlaceType),
      TelegramLocation(telegramLocationHorizontalAccuracy,
                       telegramLocationLivePeriod, telegramLocationHeading,
                       telegramLocationProximityAlertRadius, telegramLocationLatitude,
                       telegramLocationLongitude),
      TelegramContact(telegramContactPhoneNumber,
                      telegramContactFirstName, telegramContactLastName,
                      telegramContactVcard),
      TelegramVoice(telegramVoiceFileId),
      TelegramVideoNote(telegramVideoNoteFileId),
      TelegramSticker(telegramStickerFileId),
      TelegramVideo(telegramVideoFileId),
      TelegramAudio(telegramAudioFileId),
      TelegramDocument(telegramDocumentFileId),
      TelegramPhotoSize(telegramPhotoSizeFileId),
      TelegramAnimation(telegramAnimationFileId),
      TelegramMessageEntity,
      TelegramText,
      TelegramUser,
      TelegramUpdate(telegramUpdateId) )

import Telegram.Types
    ( RepeatsList,
      Repeats(..),
      StatusResult,
      Caption,
      RepeatsNum(RepeatsNum),
      ChatId,
      UpdateId(..),
      TelegramToken ) 
import Data.List ( find )

getMe :: Handle -> TelegramToken -> IO (Maybe TelegramUser)
getMe hLogger tgtoken = buildTelegramGetRequest hLogger tgtoken "getMe" []

getUpdates :: FromJSON a => Handle -> TelegramToken -> Maybe UpdateId -> IO (Maybe a)
getUpdates hLogger tgtoken (Just updId) =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", T.pack $ show $ upd_id updId), ("timeout", "10")]
getUpdates hLogger tgtoken Nothing =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", "0"), ("timeout", "10")]

getLastUpdateId :: Handle -> Maybe [TelegramUpdate] -> IO (Maybe UpdateId)
getLastUpdateId hLogger updates =
    case updates of
        Nothing -> do
            return Nothing
        Just [] -> do
            logInfo hLogger "No updates"
            return Nothing
        Just xs -> return $ Just $ nextUpd $ telegramUpdateId $ last xs
            where nextUpd (UpdateId a) = UpdateId (a+1)


updateListUsers :: RepeatsList -> RepeatsList -> RepeatsList
updateListUsers xs ((Repeats cid n):us) = updateListUsers newList us
    where
        newList = filter ((/= cid) . chat_id) xs ++ [Repeats cid n]
updateListUsers xs [] = xs

updateListUsers xs [] = xs

findRepeatNumber :: RepeatsList -> ChatId -> IO RepeatsNum
findRepeatNumber listOfUsers chatId = do
    let rep = find (\x -> chatId == chat_id x ) listOfUsers
    case rep of
      Nothing -> return $ RepeatsNum 1
      Just re -> return $ repeats_num re


sendTextMessage ::
       Handle
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
       Handle
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
       Handle
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
       Handle
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
       Handle
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
       Handle
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
       Handle -> TelegramToken -> ChatId -> TelegramSticker -> IO (Maybe StatusResult)
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
       Handle -> TelegramToken -> ChatId -> TelegramVideoNote -> IO (Maybe StatusResult)
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
       Handle -> TelegramToken -> ChatId -> TelegramContact -> IO (Maybe StatusResult)
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
       Handle -> TelegramToken -> ChatId -> TelegramLocation -> IO (Maybe StatusResult)
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

sendVenueMessage :: Handle -> TelegramToken -> ChatId -> TelegramVenue -> IO (Maybe StatusResult)
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

sendKeyboard :: Handle -> TelegramToken -> ChatId -> IO (Maybe StatusResult)
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
