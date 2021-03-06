{-# LANGUAGE OverloadedStrings #-}

module Telegram.API where

import           Data.Aeson            (FromJSON)
import qualified Data.Text             as T
import           Logger                (LogHandle, logInfo)
import           Telegram.BuildRequest (buildTelegramGetRequest,
                                        buildTelegramPostRequest)
import           Telegram.Keyboard     (keyboard)
import           Telegram.Requests     (TelegramSendAnimation (TelegramSendAnimation),
                                        TelegramSendAudio (TelegramSendAudio),
                                        TelegramSendContact (TelegramSendContact),
                                        TelegramSendDocument (TelegramSendDocument),
                                        TelegramSendLocation (TelegramSendLocation),
                                        TelegramSendMessage (TelegramSendMessage),
                                        TelegramSendPhoto (TelegramSendPhoto),
                                        TelegramSendSticker (TelegramSendSticker),
                                        TelegramSendVenue (TelegramSendVenue),
                                        TelegramSendVideo (TelegramSendVideo),
                                        TelegramSendVideoNote (TelegramSendVideoNote),
                                        TelegramSendVoice (TelegramSendVoice))
import           Telegram.Responses    (TelegramAnimation (telegramAnimationFileId),
                                        TelegramAudio (telegramAudioFileId),
                                        TelegramContact (telegramContactFirstName, telegramContactLastName, telegramContactPhoneNumber, telegramContactVcard),
                                        TelegramDocument (telegramDocumentFileId),
                                        TelegramLocation (telegramLocationHeading, telegramLocationHorizontalAccuracy, telegramLocationLatitude, telegramLocationLivePeriod, telegramLocationLongitude, telegramLocationProximityAlertRadius),
                                        TelegramMessageEntity,
                                        TelegramPhotoSize (telegramPhotoSizeFileId),
                                        TelegramSticker (telegramStickerFileId),
                                        TelegramText,
                                        TelegramUpdate (telegramUpdateId),
                                        TelegramUser,
                                        TelegramVenue (telegramVenueAddress, telegramVenueFoursquareId, telegramVenueFoursquareType, telegramVenueGooglePlaceId, telegramVenueGooglePlaceType, telegramVenueLocation, telegramVenueTitle),
                                        TelegramVideo (telegramVideoFileId),
                                        TelegramVideoNote (telegramVideoNoteFileId),
                                        TelegramVoice (telegramVoiceFileId))
import           Telegram.Types        (Caption, StatusResult, TelegramToken,
                                        UpdateId (..))
import           UsersLists            (ChatId)

getMe :: TelegramToken -> LogHandle IO -> IO (Maybe UpdateId)
getMe tgtoken hLogger = do
    ch <-
        buildTelegramGetRequest hLogger tgtoken "getMe" [] :: IO (Maybe TelegramUser)
    case ch of
        Nothing -> return Nothing
        Just _  -> return (Just $ UpdateId 0)

getUpdates ::
       FromJSON a
    => TelegramToken
    -> LogHandle IO
    -> Maybe UpdateId
    -> IO (Maybe a)
getUpdates tgtoken hLogger (Just updId) =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", T.pack $ show $ upd_id updId), ("timeout", "10")]
getUpdates tgtoken hLogger Nothing =
    buildTelegramGetRequest
        hLogger
        tgtoken
        "getUpdates"
        [("offset", "0"), ("timeout", "10")]

getLastUpdateId :: Maybe [TelegramUpdate] -> LogHandle IO -> IO (Maybe UpdateId)
getLastUpdateId updates hLogger =
    case updates of
        Nothing -> do
            return Nothing
        Just [] -> do
            logInfo hLogger "No updates"
            return Nothing
        Just xs -> return $ Just $ nextUpd $ telegramUpdateId $ last xs
  where
    nextUpd (UpdateId x) = UpdateId (x + 1)

sendTextMessage ::
       LogHandle IO
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
       LogHandle IO
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
       LogHandle IO
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
       LogHandle IO
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
       LogHandle IO
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
       LogHandle IO
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
       LogHandle IO
    -> TelegramToken
    -> ChatId
    -> TelegramSticker
    -> IO (Maybe StatusResult)
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
       LogHandle IO
    -> TelegramToken
    -> ChatId
    -> TelegramVideoNote
    -> IO (Maybe StatusResult)
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
       LogHandle IO
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
       LogHandle IO
    -> TelegramToken
    -> ChatId
    -> TelegramContact
    -> IO (Maybe StatusResult)
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
       LogHandle IO
    -> TelegramToken
    -> ChatId
    -> TelegramLocation
    -> IO (Maybe StatusResult)
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

sendVenueMessage ::
       LogHandle IO
    -> TelegramToken
    -> ChatId
    -> TelegramVenue
    -> IO (Maybe StatusResult)
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

sendKeyboard ::
       LogHandle IO -> TelegramToken -> ChatId -> IO (Maybe StatusResult)
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
       LogHandle IO
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
