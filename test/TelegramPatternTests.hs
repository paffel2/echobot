{-# OPTIONS_GHC -Wno-missing-fields #-}

module TelegramPatternTests where

import           Data.Functor.Identity   (Identity)
import qualified Data.Text.IO            as TIO
import           Logger                  (LogHandle (..), Priority (Debug))
import           Telegram.Echo           (echo)
import           Telegram.Responses      (TelegramCallbackQuery (..),
                                          TelegramMessage (TelegramMessage),
                                          TelegramUpdate (TelegramUpdate),
                                          TelegramUser (TelegramUser, telegramUserId))
import           Telegram.TelegramHandle (TelegramHandle (..))
import           Telegram.Types          (HelpMessage (HelpMessage),
                                          StatusResult (StatusResult),
                                          TelegramToken (TelegramToken),
                                          UpdateId (UpdateId))
import           Test.Hspec              (describe, hspec, it, shouldBe)
import           UsersLists              (ChatId (ChatId), Repeats (Repeats),
                                          RepeatsNum (RepeatsNum))

logHandle :: LogHandle Identity
logHandle =
    LogHandle {priority = Debug, Logger.log = \prior message -> return ()}

telegramHandle :: TelegramHandle Identity
telegramHandle =
    TelegramHandle
        { getMe = \logHandle token -> return Nothing
        , sendKeyboard = \logHandle token chatId -> return Nothing
        , getUpdates = \logHandle token updateId -> return Nothing
        , getLastUpdateId = \logHandle updates -> return Nothing
        , sendTextMessage =
              \logHandle token chatId text entities -> return Nothing
        , sendAnimationMessage =
              \logHandle token chatId animation caption -> return Nothing
        , sendAudioMessage =
              \logHandle token chatId audio caption -> return Nothing
        , sendVideoMessage =
              \logHandle token chatId video caption -> return Nothing
        , sendDocumentMessage =
              \logHandle token chatId document caption -> return Nothing
        , sendPhotoMessage =
              \logHandle token chatId photo caption -> return Nothing
        , sendStickerMessage = \logHandle token chatId sticker -> return Nothing
        , sendVideoNoteMessage =
              \logHandle token chatId videoNote -> return Nothing
        , sendVoiceMessage =
              \logHandle token chatId voice caption -> return Nothing
        , sendContactMessage = \logHandle token chatId contact -> return Nothing
        , sendLocationMessage =
              \logHandle token chatId location -> return Nothing
        , sendVenueMessage = \logHandle token chatId venue -> return Nothing
        }

echoTelegramTests :: IO ()
echoTelegramTests =
    hspec $ do
        describe "Testing vk echo function" $ do
            it
                "Should not change UpdateId and repeatsList, because don't have updates" $ do
                echo
                    logHandle
                    telegramHandle
                    (TelegramToken "token")
                    (Just $ UpdateId 0)
                    (HelpMessage "help_message")
                    [] `shouldBe`
                    return (Nothing, [])
            it
                "Should return new UpdateId and empty list of the users, because server has new update without changing the list of users " $ do
                echo
                    logHandle
                    (telegramHandle
                         { getLastUpdateId =
                               \logHandle updates -> return (Just $ UpdateId 1)
                         })
                    (TelegramToken "token")
                    (Just $ UpdateId 0)
                    (HelpMessage "help_message")
                    [] `shouldBe`
                    return (Just $ UpdateId 1, [])
            it
                "Should return new UpdateId and not empty list of the users, \
                 \because server has new update where user changed the number of repetitions for the first time" $ do
                echo
                    logHandle
                    (telegramHandle
                         { getUpdates =
                               \logHandle token updateId ->
                                   return $ Just [numsUpdate]
                         , sendTextMessage =
                               \logHandle token chatId text entities ->
                                   return $ Just $ StatusResult 200
                         , getLastUpdateId =
                               \logHandle updates -> return (Just $ UpdateId 1)
                         })
                    (TelegramToken "token")
                    (Just $ UpdateId 0)
                    (HelpMessage "help_message")
                    [] `shouldBe`
                    return
                        (Just $ UpdateId 1, [Repeats (ChatId 1) (RepeatsNum 2)])
            it
                "Should return new UpdateId and not empty list of the users, \
                 \because server has new update where user changed the number" $ do
                echo
                    logHandle
                    (telegramHandle
                         { getUpdates =
                               \logHandle token updateId ->
                                   return $ Just [numsUpdate]
                         , sendTextMessage =
                               \logHandle token chatId text entities ->
                                   return $ Just $ StatusResult 200
                         , getLastUpdateId =
                               \logHandle updates -> return (Just $ UpdateId 1)
                         })
                    (TelegramToken "token")
                    (Just $ UpdateId 0)
                    (HelpMessage "help_message")
                    [Repeats (ChatId 1) (RepeatsNum 5)] `shouldBe`
                    return
                        (Just $ UpdateId 1, [Repeats (ChatId 1) (RepeatsNum 2)])
            it
                "Should not change UpdateId and repeatsList, because don't have updates and UpdateId is Nothing" $ do
                echo
                    logHandle
                    telegramHandle
                    (TelegramToken "token")
                    Nothing
                    (HelpMessage "help_message")
                    [] `shouldBe`
                    return (Nothing, [])
            it
                "Should return new UpdateId and new list of users, because echo function did not receive an update ID, \
                 \but get new update, where get new list of users and new UpdateId." $ do
                echo
                    logHandle
                    (telegramHandle
                         { getUpdates =
                               \logHandle token updateId ->
                                   return $ Just [numsUpdate]
                         , sendTextMessage =
                               \logHandle token chatId text entities ->
                                   return $ Just $ StatusResult 200
                         , getLastUpdateId =
                               \logHandle updates -> return (Just $ UpdateId 1)
                         })
                    (TelegramToken "token")
                    Nothing
                    (HelpMessage "help_message")
                    [] `shouldBe`
                    return
                        (Just $ UpdateId 1, [Repeats (ChatId 1) (RepeatsNum 2)])

numsUpdate :: TelegramUpdate
numsUpdate = TelegramUpdate (UpdateId 0) Nothing (Just callback)

callback :: TelegramCallbackQuery
callback =
    TelegramCallbackQuery
        { telegramCallbackQueryId = ""
        , telegramCallbackQueryFrom = someUser
        , telegramCallbackQueryMessage = Just someMessage
        , telegramCallbackQueryChatInstance = ""
        , telegramCallbackQueryData = Just $ RepeatsNum 2
        }

someUser :: TelegramUser
someUser = TelegramUser {telegramUserId = ChatId 1}

someMessage :: TelegramMessage
someMessage = TelegramMessage {}
