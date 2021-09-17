module VkPatternTests where

import Data.Functor.Identity (Identity)
import Logger (Handle(..), Priority(Debug))
import Test.Hspec (describe, hspec, it, shouldBe)
import Vk.Echo (echo)
import Vk.Responses
    ( VkItem(VkItem, vkItemAttachments, vkItemFromId, vkItemFwdMessages,
       vkItemGeo, vkItemId, vkItemImportant, vkItemPayload, vkItemText)
    , VkMessages(VkMessages)
    , VkResponseType(Server)
    )
import Vk.VkHandle (VKHandle(..))

logHandle :: Handle Identity
logHandle = Handle {priority = Debug, Logger.log = \prior message -> return ()}

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
            it "Should return Nothing, because don't have update " $ do
                echo logHandle vkHandle "token" "help_message" [] 1 1 `shouldBe`
                    return Nothing
            it
                "Should return (Just ((1,1),[]), because server return ts and pts parameters" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (1, 1))
                         })
                    "token"
                    "help_message"
                    []
                    1
                    1 `shouldBe`
                    return (Just ((1, 1), []))
            it
                "Should return (Just ((1,1),[(1,2)]), because server return ts and pts parameters and one user change repeat parameter" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (1, 1))
                         , sendMessageRepeatText =
                               \logHandle token reapeatsList vktem ->
                                   return $ Just (1, 2)
                         , getLongPollHistory =
                               \logHandle token ts pts ->
                                   return $ Just vkResponse
                         })
                    "token"
                    "help_message"
                    []
                    1
                    1 `shouldBe`
                    return (Just ((1, 1), [(1, 2)]))
            it
                "Should return (Just ((1,1),[(1,2)]), because server return ts and pts parameters and one user change already existing repeat parameter" $ do
                echo
                    logHandle
                    (vkHandle
                         { getTsAndPts =
                               \logHandle token -> return (Just (1, 1))
                         , sendMessageRepeatText =
                               \logHandle token reapeatsList vktem ->
                                   return $ Just (1, 2)
                         , getLongPollHistory =
                               \logHandle token ts pts ->
                                   return $ Just vkResponse
                         })
                    "token"
                    "help_message"
                    [(1, 5)]
                    1
                    1 `shouldBe`
                    return (Just ((1, 1), [(1, 2)]))

vkResponse :: VkResponseType
vkResponse = Server (Just "") (Just "") (Just 1) (Just 1) (Just 2) (Just vkMess)

vkMess :: VkMessages
vkMess =
    VkMessages
        1
        [ VkItem
              { vkItemId = Nothing
              , vkItemFromId = 1
              , vkItemText = ""
              , vkItemAttachments = []
              , vkItemImportant = Nothing
              , vkItemGeo = Nothing
              , vkItemFwdMessages = Nothing
              , vkItemPayload = Just "2"
              }
        ]
