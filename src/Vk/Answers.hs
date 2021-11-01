{-# LANGUAGE NamedFieldPuns #-}

module Vk.Answers where

import           Data.Maybe   (catMaybes)
import           Logger       (LogHandle, logError)
import           UsersLists   (HelpMessage, RepeatsList, updateListUsers)
import           Vk.Responses (VkMessages (vkMessagesItems),
                               VkResponseType (Server, serverMessages))
import           Vk.Types     (VkToken)
import           Vk.VkHandle  (VKHandle (repeatMessage, sendKeyboardVk, sendMessageHelp, sendMessageRepeatText))

answers ::
       Monad m
    => VKHandle m
    -> VkToken
    -> LogHandle m
    -> HelpMessage
    -> Maybe VkResponseType
    -> RepeatsList
    -> m RepeatsList
answers hVK vktoken hLogger help_message (Just Server {serverMessages = (Just messages)}) xs =
    answer $ vkMessagesItems messages
  where
    answer list = do
        mapM_ (repeatMessage hVK hLogger vktoken xs) list
        mapM_ (sendKeyboardVk hVK hLogger vktoken) list
        mapM_ (sendMessageHelp hVK hLogger vktoken help_message) list
        update <-
            catMaybes <$>
            mapM (sendMessageRepeatText hVK hLogger vktoken xs) list
        return $ updateListUsers xs update
answers _ _ hLogger _ (Just _) xs = do
    logError hLogger "Unexcepted error"
    return xs
answers _ _ hLogger _ Nothing xs = do
    logError hLogger "No updates"
    return xs
