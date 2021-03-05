{-# LANGUAGE OverloadedStrings #-}
module API where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson
import GHC.Generics
import Network.HTTP.Req
import Control.Monad.IO.Class
import TelegramResponses
import Data.Aeson.Types
import Control.Exception (throwIO)
import Data.List
--import Data.Maybe


instance MonadHttp IO where
  handleHttpException = throwIO


type TelegramToken = String

type BotType = String


testToken :: TelegramToken
testToken = "1431530804:AAH5bSr9xr8o3WQlF55hnmpYZYtktn-rzWY"

-----------------------------------------------------------------
data MyConfig = MyConfig { botType :: String
                         , token :: TelegramToken
                         , lastupdate :: Int
                         , help :: T.Text
                         } deriving Show

instance FromJSON MyConfig where
    parseJSON (Object v) = 
        MyConfig <$> v .: "bot_type"
                 <*> v .: "token"
                 <*> v .: "last_update"
                 <*> v .: "help"

readConfig :: IO (Maybe MyConfig)
readConfig = do
    rawJSON <- B.readFile "D:/Haskell/echobot/config.json"
    let result = decodeStrict rawJSON :: Maybe MyConfig
    --TIO.putStrLn $ case result of
    --    Nothing -> "Invalid JSON"
    --    Just a -> help a
    return result
-----------------------------------------------------------------


buildTelegramGetRequest :: FromJSON a => TelegramToken -> String -> [(T.Text, T.Text)] -> IO (Either String a)
buildTelegramGetRequest token url params = do
    response <- responseBody <$> request
    return $ parseResult response
        where request = req
                            GET
                            (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
                            NoReqBody
                            jsonResponse
                            param
              param = buildParams params
              parseResult r = case parseEither parseJSON r of
                Right (TelegramResponse True _ (Just result)) -> Right result
                Right (TelegramResponse False (Just errMess) _) -> Left "1 er"
                Right (TelegramResponse True errMess Nothing) -> Left $ "no result"
                Left errMess -> Left "2 er"

buildParams :: (QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildParams [] = mempty
buildParams params = mconcat $ fmap (uncurry (=:)) params


getUpdates :: TelegramToken -> IO (Either String [TelegramUpdate])
getUpdates token = buildTelegramGetRequest token "getUpdates" [("offset", "-1"), ("timeout", "10")]

getUpdates' token (Just updId) = buildTelegramGetRequest token "getUpdates" [("offset", T.pack $ show updId), ("timeout", "10")]
getUpdates' token Nothing = buildTelegramGetRequest token "getUpdates" [("offset", "0"), ("timeout", "10")]

getMe :: TelegramToken -> IO (Either String TelegramUser)
getMe token = buildTelegramGetRequest token "getMe" []




buildTelegramPostRequest :: ToJSON b => String -> String -> b -> [(T.Text,T.Text)] -> IO (Int)
buildTelegramPostRequest token url body params = runReq defaultHttpConfig $ do
    r <- req
        POST
        (https "api.telegram.org" /: T.pack ("bot" ++ token) /: T.pack url)
        (ReqBodyJson body)
        jsonResponse
        param
    return $ responseStatusCode (r :: JsonResponse Value)
        where param = buildParams params


--sendMessageTextTest' :: String -> IO (Int)
--sendMessageTextTest' token = buildTelegramPostRequest token "sendMessage" (TelegramSendMessage 274864287 "привет" Nothing ) []

sendMessage :: String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO Int
sendMessage token chatId text ent = buildTelegramPostRequest token "sendMessage" (TelegramSendMessage chatId text ent Nothing) []

sendAnimation token chatId anim cap = buildTelegramPostRequest token "sendAnimation" (TelegramSendAnimation chatId anim cap) []

sendAudio token chatId audio cap = buildTelegramPostRequest token "sendAudio" (TelegramSendAudio chatId audio cap) []

sendDocument token chatId doc cap = buildTelegramPostRequest token "sendDocument" (TelegramSendDocument chatId doc cap) []

sendPhoto token chatId photo cap = buildTelegramPostRequest token "sendPhoto" (TelegramSendPhoto chatId photo cap) []

sendVideo token chatId video cap = buildTelegramPostRequest token "sendVideo" (TelegramSendVideo chatId video cap) []

sendSticker token chatId sticker = buildTelegramPostRequest token "sendSticker" (TelegramSendSticker chatId sticker) []

sendVideoNote token chatId video = buildTelegramPostRequest token "sendVideoNote" (TelegramSendVideoNote chatId video) []

sendVoice token chatId voice cap = buildTelegramPostRequest token "sendVoice" (TelegramSendVoice chatId voice cap) []

sendContact token chatId phoneNum fname lname vcard = buildTelegramPostRequest token "sendContact" (TelegramSendContact chatId phoneNum fname lname vcard) []

sendLocation token chatId lat long horac lp hea par = buildTelegramPostRequest token "sendLocation" (TelegramSendLocation chatId lat long horac lp hea par) []

sendVenue token chatId lat long title address fsid fstype gpid gptype = buildTelegramPostRequest token "sendVenue" (TelegramSendVenue chatId lat long title address fsid fstype gpid gptype) []

{-buttons = [[x]| x <-(TelegramKeyboardButton <$> ["1","2","3","4","ы"])]

keyboard = TelegramReplyKeyboardMarkup buttons (Just False)

sendKeyboard token chatId = buildTelegramPostRequest token "sendMessage" (TelegramSendMessage chatId "типа клава" Nothing (Just keyboard)) []

removeKeyboard = buildTelegramPostRequest testToken "sendMessage" (TelegramSendMessage 274864287 "1" Nothing (Just (TelegramReplyKeyboardRemove True))) []-}

buttons =  [ [TelegramInlineKeyboardButton x x] | x <- ["1","2","3","4","5"]]

keyboard = TelegramInlineKeyboardMarkup buttons

sendKeyboard token chatId = buildTelegramPostRequest token "sendMessage" (TelegramSendMessage chatId "типа клава" Nothing (Just keyboard)) []
------------------------------------------------------------------------------------------------------------------------------------------

{-echo = do
    updates <- getUpdates testToken
    answers updates
    putStrLn "done"-}

answers (Right upd) list = mapM (answer list) upd

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/repeat") _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    n <- findRepeatNumber list chatId
    sendKeyboard testToken chatId
    return Nothing
        where chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just "/help") _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    sendMessage testToken chatId "helptext описание" Nothing
    return Nothing
        where chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ (Just text) _ _ _ _ _ _ _ _ _ _ _ _ _))  _) = do
    n <- findRepeatNumber list chatId
    repeatSendMessage n testToken chatId ansText entities
    return Nothing
        where ansText = text
              entities = telegramMessageEntities message
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ (Just anim) _ _ _ _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendAnimation n testToken chatId animid cap
    return Nothing
        where cap = telegramMessageCaption message
              animid = telegramAnimationFileId anim
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just audio) _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendAudio n testToken chatId audioid cap
    return Nothing
        where cap = telegramMessageCaption message
              audioid = telegramAudioFileId audio
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ Nothing _ (Just doc) _ _ _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendDocument n testToken chatId docid cap
    return Nothing
        where cap = telegramMessageCaption message
              docid = telegramDocumentFileId doc
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (photo:_)) _ _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendPhoto n testToken chatId photoid cap
    return Nothing
        where cap = telegramMessageCaption message
              photoid = telegramPhotoSizeFileId photo
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just video) _ _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVideo n testToken chatId videoid cap
    return Nothing
        where cap = telegramMessageCaption message
              videoid = telegramVideoFileId video
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just sticker) _ _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendSticker n testToken chatId stickerid
    return Nothing
        where stickerid = telegramStickerFileId sticker
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just videoNote) _ _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVideoNote n testToken chatId videoid
    return Nothing
        where videoid = telegramVideoNoteFileId videoNote
              chatId = telegramChatId $ telegramMessageChat message


answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just voice) _ _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVoice n testToken chatId voiceid cap
    return Nothing
        where cap = telegramMessageCaption message
              voiceid = telegramVoiceFileId voice
              chatId = telegramChatId $ telegramMessageChat message

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _(Just contact) _ _)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendContact n testToken chatId phoneNumber fname lname vcard
    return Nothing
      where chatId = telegramChatId $ telegramMessageChat message
            phoneNumber = telegramContactPhoneNumber contact
            fname = telegramContactFirstName contact
            lname = telegramContactLastName contact
            vcard = telegramContactVcard contact

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just location) Nothing)) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendLocation n testToken chatId lat long horac lp hea par
    return Nothing
      where chatId = telegramChatId $ telegramMessageChat message
            lat = telegramLocationLatitude location
            long = telegramLocationLongitude location
            horac = telegramLocationHorizontalAccuracy location
            lp = telegramLocationLivePeriod location
            hea = telegramLocationHeading location
            par = telegramLocationProximityAlertRadius location

answer list (TelegramUpdate _ (Just message@(TelegramMessage _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just venue))) _ ) = do
    n <- findRepeatNumber list chatId
    repeatSendVenue n testToken chatId lat long title address fsid fstype gpid gptype
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




answer list (TelegramUpdate _ _(Just callback@(TelegramCallbackQuery _ user (Just message) _ (Just dat)))) = do
    sendMessage testToken chatId text Nothing
    putStrLn "sended"
    return $ Just(chatId, read dat :: Int)
        where chatId = telegramUserId user
              text = "число повторов теперь равно " ++ dat

------------------------------------------------------------------------------------------------------------------------------------------}
repeatSendMessage :: Int -> String -> Int -> String -> Maybe [TelegramMessageEntity] -> IO ()
repeatSendMessage n token chatId text entities | n > 0 = do 
                                                      sendMessage token chatId text entities
                                                      putStrLn "sended"
                                                      repeatSendMessage (n-1) token chatId text entities
                                               | otherwise = putStrLn "all sended"

repeatSendAnimation :: Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendAnimation n token chatId anim cap | n > 0 = do
                                                        sendAnimation token chatId anim cap
                                                        putStrLn "sended"
                                                        repeatSendAnimation (n-1) token chatId anim cap
                                            | otherwise = putStrLn "all sended"

repeatSendAudio :: Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendAudio n token chatId audio cap | n > 0 = do
                                                        sendAudio token chatId audio cap
                                                        putStrLn "sended"
                                                        repeatSendAudio (n-1) token chatId audio cap
                                         | otherwise = putStrLn "all sended"

repeatSendDocument :: Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendDocument n token chatId doc cap | n > 0 = do
                                                        sendDocument token chatId doc cap
                                                        putStrLn "sended"
                                                        repeatSendDocument (n-1) token chatId doc cap
                                          | otherwise = putStrLn "all sended"

repeatSendPhoto ::  Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendPhoto n token chatId photo cap | n > 0 = do
                                                        sendPhoto token chatId photo cap
                                                        putStrLn "sended"
                                                        repeatSendPhoto (n-1) token chatId photo cap
                                         | otherwise = putStrLn "all sended"

repeatSendVideo ::  Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendVideo n token chatId video cap | n > 0 = do
                                                        sendVideo token chatId video cap
                                                        putStrLn "sended"
                                                        repeatSendVideo (n-1) token chatId video cap
                                         | otherwise = putStrLn "all sended"

repeatSendSticker ::  Int -> String -> Int -> String -> IO ()
repeatSendSticker n token chatId sticker | n > 0 = do
                                                        sendSticker token chatId sticker
                                                        putStrLn "sended"
                                                        repeatSendSticker (n-1) token chatId sticker
                                         | otherwise = putStrLn "all sended"

repeatSendVideoNote ::  Int -> String -> Int -> String -> IO ()
repeatSendVideoNote n token chatId videonote | n > 0 = do
                                                        sendVideoNote token chatId videonote
                                                        putStrLn "sended"
                                                        repeatSendVideoNote (n-1) token chatId videonote
                                             | otherwise = putStrLn "all sended"

repeatSendVoice :: Int -> String -> Int -> String -> Maybe String -> IO ()
repeatSendVoice n token chatId voice cap | n > 0 = do
                                                        sendVoice token chatId voice cap
                                                        putStrLn "sended"
                                                        repeatSendVoice (n-1) token chatId voice cap
                                         | otherwise = putStrLn "all sended"

repeatSendContact :: Int -> String -> Int -> String -> String -> Maybe String -> Maybe String -> IO ()
repeatSendContact n token chatId phoneNum fname lname vcard | n > 0 = do
                                                                sendContact token chatId phoneNum fname lname vcard
                                                                putStrLn "sended"
                                                                repeatSendContact (n-1) token chatId phoneNum fname lname vcard
                                                            | otherwise = putStrLn "all sended"

repeatSendLocation :: Int -> String -> Int -> Double -> Double -> Maybe Double -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
repeatSendLocation n token chatId lat long horac lp hea par   | n > 0 = do
                                                                sendLocation token chatId lat long horac lp hea par
                                                                putStrLn "sended"
                                                                repeatSendLocation (n-1) token chatId lat long horac lp hea par
                                                              | otherwise = putStrLn "all sended"

repeatSendVenue :: Int -> String -> Int -> Double -> Double -> String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO ()
repeatSendVenue n token chatId lat long title address fsid fstype gpid gptype   | n > 0 = do
                                                                                    sendVenue token chatId lat long title address fsid fstype gpid gptype
                                                                                    putStrLn "sended"
                                                                                    repeatSendVenue (n-1) token chatId lat long title address fsid fstype gpid gptype
                                                                                | otherwise = putStrLn "all sended"
              
---------------------------------------------------------------------------------------------------------------------------------
echo' :: Maybe Int -> [(Int,Int)]-> IO a
echo' updateId listOfUsers = do
  updates <- getUpdates' testToken updateId
  b <- answers updates listOfUsers
  let listOfUsers' = updateListUsers listOfUsers b
  putStrLn $ show listOfUsers'
  nextUpdateID <- getLastUpdateId updates
  echo' nextUpdateID listOfUsers'

getLastUpdateId :: Either String [TelegramUpdate] -> IO (Maybe Int)
getLastUpdateId updates = case updates of
    Left err -> do
        putStrLn "err"
        return Nothing
    Right [] -> do
        putStrLn "no updates"
        return Nothing
    Right xs -> return $ Just $ (+ 1) $ telegramUpdateId $ last xs



-----------------------------------------------------------------------------------------

{-updateListUsers :: [(Int, Int)] -> [Maybe (Int, Int)] -> [(Int,Int)]
updateListUsers xs (u:us) =  case update of
    Nothing -> xs
    Just (cid, n) -> newlist' ++ [(cid, n)] where
                              newlist' = filter ((/=cid) . fst) xs-}
--updateListUsers :: [(Int,Int)] -> [Maybe (Int,Int)] -> [[(Int,Int)]]
updateListUsers xs (u:us) = updateListUsers newList us where
    newList = case u of
        Nothing -> xs
        Just (cid, n) -> newlist' ++ [(cid, n)] where
                              newlist' = filter ((/=cid) . fst) xs
updateListUsers xs [] = xs

testList :: [(Int, Int)]
testList = [(1,5), (2,6), (3,7), (274864287, 2)]

testUpdate :: [Maybe (Int,Int)]
testUpdate = [Nothing, Just(4,5),Just(2,5)]

findRepeatNumber :: [(Int, Int)] -> Int -> IO (Int)
findRepeatNumber listOfUsers chatId = do
  let n = lookup chatId listOfUsers
  case n of 
    Just x -> do
                 putStrLn "user founded"
                 return x
    Nothing -> do 
                 putStrLn "user not found"
                 return 1

