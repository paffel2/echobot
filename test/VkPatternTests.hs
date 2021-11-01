module VkPatternTests where

import           Data.Functor.Identity (Identity)
import           Logger                (LogHandle (..), Priority (Debug))
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           UsersLists            (ChatId (ChatId), Repeats (Repeats),
                                        RepeatsNum (RepeatsNum))
import           Vk.Echo               (echo)
import           Vk.Responses          (VkItem (VkItem, vkItemAttachments, vkItemFromId, vkItemFwdMessages, vkItemGeo, vkItemId, vkItemImportant, vkItemPayload, vkItemText),
                                        VkMessages (VkMessages),
                                        VkResponseType (Server))
import           Vk.Types              (HelpMessage (HelpMessage), Pts (Pts),
                                        Ts (Ts), VkToken (VkToken))
import           Vk.VkHandle           (VKHandle (..))

logHandle :: LogHandle Identity
logHandle =
    LogHandle {priority = Debug, Logger.log = \prior message -> return ()}

vkHandle :: VKHandle Identity
vkHandle =
    VKHandle
        { getLongPollServer = \logHandle token -> return Nothing
        , getLongPollHistory = \logHandle token ts pts -> return Nothing
        , getTsAndPts = \logHandle token -> return Nothing
        , sendMessageText = \logHandle token vkitem -> return ()
        , sendKeyboardVk = \logHandle token vkitem -> return ()
        , sendMessageRepeatText =
              \logHandle token reapeatsList vktem -> return Nothing
        , repeatMessage = \logHandle token reapeatsList vktem -> return ()
        , sendMessageHelp = \logHandle token help_message vkitem -> return ()
        }

echoVkTests :: IO ()
echoVkTests =
    hspec $ do
        describe "Testing vk echo function" $ do
            it
                "Should not change UpdateId and repeatsList, because don't have updates" $ do
                echo
                    logHandle
                    vkHandle
                    (VkToken "token")
                    (HelpMessage "help_message")
                    []
                    (Ts 1)
                    (Pts 1) `shouldBe`
                    return Nothing
            it
                "Should return new Ts and Pts, because server return Ts and Pts parameters" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (Ts 1, Pts 2))
                         })
                    (VkToken "token")
                    (HelpMessage "help_message")
                    []
                    (Ts 1)
                    (Pts 1) `shouldBe`
                    return (Just ((Ts 1, Pts 2), []))
            it
                "Should return new Ts and Pts, and new list of users, because server return ts and pts parameters and one user change repeat parameter" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (Ts 1, Pts 2))
                         , sendMessageRepeatText =
                               \logHandle token reapeatsList vktem ->
                                   return $
                                   Just $ Repeats (ChatId 1) (RepeatsNum 2)
                         , getLongPollHistory =
                               \logHandle token ts pts ->
                                   return $ Just vkResponse
                         })
                    (VkToken "token")
                    (HelpMessage "help_message")
                    []
                    (Ts 1)
                    (Pts 1) `shouldBe`
                    return
                        (Just
                             ( (Ts 1, Pts 2)
                             , [Repeats (ChatId 1) (RepeatsNum 2)]))
            it
                "Should return new Ts and Pts, and updated list of users, because server return ts and pts parameters and one user change already existing repeat parameter" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (Ts 1, Pts 2))
                         , sendMessageRepeatText =
                               \logHandle token reapeatsList vktem ->
                                   return $
                                   Just $ Repeats (ChatId 1) (RepeatsNum 2)
                         , getLongPollHistory =
                               \logHandle token ts pts ->
                                   return $ Just vkResponse
                         })
                    (VkToken "token")
                    (HelpMessage "help_message")
                    [Repeats (ChatId 1) (RepeatsNum 5)]
                    (Ts 1)
                    (Pts 1) `shouldBe`
                    return
                        (Just
                             ( (Ts 1, Pts 2)
                             , [Repeats (ChatId 1) (RepeatsNum 2)]))

vkResponse :: VkResponseType
vkResponse =
    Server
        (Just "")
        (Just "")
        (Just $ Ts 1)
        (Just $ Pts 2)
        (Just 2)
        (Just vkMess)

vkMess :: VkMessages
vkMess =
    VkMessages
        1
        [ VkItem
              { vkItemId = Nothing
              , vkItemFromId = ChatId 1
              , vkItemText = ""
              , vkItemAttachments = []
              , vkItemImportant = Nothing
              , vkItemGeo = Nothing
              , vkItemFwdMessages = Nothing
              , vkItemPayload = Just $ RepeatsNum 2
              }
        ]
