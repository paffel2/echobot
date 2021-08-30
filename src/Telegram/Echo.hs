module Telegram.Echo where

import Logger (Handle, logDebug, logError)
import Telegram.BuildRequest (TelegramToken)
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
    , TelegramPhotoSize(telegramPhotoSizeFileId)
    , TelegramSticker(telegramStickerFileId)
    , TelegramUpdate(TelegramUpdate)
    , TelegramUser(telegramUserId)
    , TelegramVenue(telegramVenueAddress, telegramVenueFoursquareId,
              telegramVenueFoursquareType, telegramVenueGooglePlaceId,
              telegramVenueGooglePlaceType, telegramVenueLocation,
              telegramVenueTitle)
    , TelegramVideo(telegramVideoFileId)
    , TelegramVideoNote(telegramVideoNoteFileId)
    , TelegramVoice(telegramVoiceFileId)
    )
import Telegram.TelegramHandle
    ( TelegramHandle(findRepeatNumber, getLastUpdateId, getUpdates,
               repeatSendAnimation, repeatSendAudio, repeatSendContact,
               repeatSendDocument, repeatSendLocation, repeatSendMessage,
               repeatSendPhoto, repeatSendSticker, repeatSendVenue,
               repeatSendVideo, repeatSendVideoNote, repeatSendVoice,
               sendKeyboard, sendMessage, updateListUsers)
    )

echo ::
       Handle
    -> TelegramHandle
    -> TelegramToken
    -> Maybe Int
    -> String
    -> [(Int, Int)]
    -> IO ()
echo hLogger' hTelegram' tgtoken' updateId help_message' listOfUsers = do
    updates <- getUpdates hTelegram' hLogger' tgtoken' updateId
    listOfUsersUpd <-
        answers hLogger' hTelegram' help_message' tgtoken' updates listOfUsers
    let newListOfUsers = updateListUsers hTelegram' listOfUsers listOfUsersUpd
    nextUpdateID <- getLastUpdateId hTelegram' hLogger' updates
    echo hLogger' hTelegram' tgtoken' nextUpdateID help_message' newListOfUsers
  where
    answers hLogger hTelegram help_message tgtoken (Just upd) list =
        mapM (answer hLogger hTelegram help_message tgtoken list) upd
    answers hLogger _ _ _ _ _ = do
        logError hLogger "Something wrong"
        return [Nothing]
    answer hLogger hTelegram _ tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
        status <- sendKeyboard hTelegram hLogger tgtoken chatId
        case status of
            Nothing -> logError hLogger "Keyboard not send"
            Just _ -> logDebug hLogger "Keyboard sended"
        return Nothing
      where
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram help_message tgtoken _ (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
        status <-
            sendMessage hTelegram hLogger tgtoken chatId help_message Nothing
        case status of
            Nothing -> logError hLogger "Help message not sended"
            Just _ -> logDebug hLogger "Help message sended"
        return Nothing
      where
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just text) _ _ _ _ _ _ _ _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <-
            repeatSendMessage
                hTelegram
                hLogger
                n
                tgtoken
                chatId
                ansText
                entities
        return Nothing
      where
        ansText = text
        entities = telegramMessageEntities message
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ (Just anim) _ _ _ _ _ _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendAnimation hTelegram hLogger n tgtoken chatId animid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        animid = telegramAnimationFileId anim
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just audio) _ _ _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendAudio hTelegram hLogger n tgtoken chatId audioid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        audioid = telegramAudioFileId audio
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ Nothing _ (Just doc) _ _ _ _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendDocument hTelegram hLogger n tgtoken chatId docid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        docid = telegramDocumentFileId doc
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (photo:_)) _ _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendPhoto hTelegram hLogger n tgtoken chatId photoid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        photoid = telegramPhotoSizeFileId photo
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just video) _ _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendVideo hTelegram hLogger n tgtoken chatId videoid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        videoid = telegramVideoFileId video
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just sticker) _ _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendSticker hTelegram hLogger n tgtoken chatId stickerid
        return Nothing
      where
        stickerid = telegramStickerFileId sticker
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just videoNote) _ _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendVideoNote hTelegram hLogger n tgtoken chatId videoid
        return Nothing
      where
        videoid = telegramVideoNoteFileId videoNote
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just voice) _ _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <- repeatSendVoice hTelegram hLogger n tgtoken chatId voiceid cap
        return Nothing
      where
        cap = telegramMessageCaption message
        voiceid = telegramVoiceFileId voice
        chatId = telegramChatId $ telegramMessageChat message
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just contact) _ _)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <-
            repeatSendContact
                hTelegram
                hLogger
                n
                tgtoken
                chatId
                phoneNumber
                fname
                lname
                vcard
        return Nothing
      where
        chatId = telegramChatId $ telegramMessageChat message
        phoneNumber = telegramContactPhoneNumber contact
        fname = telegramContactFirstName contact
        lname = telegramContactLastName contact
        vcard = telegramContactVcard contact
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just location) Nothing)) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <-
            repeatSendLocation
                hTelegram
                hLogger
                n
                tgtoken
                chatId
                lat
                long
                horac
                lp
                hea
                par
        return Nothing
      where
        chatId = telegramChatId $ telegramMessageChat message
        lat = telegramLocationLatitude location
        long = telegramLocationLongitude location
        horac = telegramLocationHorizontalAccuracy location
        lp = telegramLocationLivePeriod location
        hea = telegramLocationHeading location
        par = telegramLocationProximityAlertRadius location
    answer hLogger hTelegram _ tgtoken list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just venue))) _) = do
        n <- findRepeatNumber hTelegram list chatId
        _ <-
            repeatSendVenue
                hTelegram
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
    answer hLogger hTelegram _ tgtoken _ (TelegramUpdate _ _ (Just (TelegramCallbackQuery _ user (Just _) _ (Just dat)))) = do
        status <- sendMessage hTelegram hLogger tgtoken chatId text Nothing
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
    answer _ _ _ _ _ _ = return Nothing
