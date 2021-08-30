module Vk.Echo where

import Control.Concurrent (threadDelay)
import Logger (Handle, logError)
import Vk.BuildRequests (VkToken)
import Vk.Responses (VkMessages(vkMessagesItems), VkResponseType(Server))
import Vk.VkHandle
    ( VKHandle(getLongPollHistory, getTsAndPts, repeatMessage,
         sendKeyboardVk, sendMessageHelp, sendMessageRepeatText,
         updateListUsers)
    )

echo ::
       Handle
    -> VKHandle
    -> VkToken
    -> String
    -> [(Int, Int)]
    -> Int
    -> Int
    -> IO ()
echo hLogger' hVK' vktoken' help_message' listOfUsers' ts' pts' = do
    updates <- getLongPollHistory hVK' hLogger' vktoken' ts' pts'
    newListOfUsers <-
        answer hLogger' hVK' vktoken' help_message' updates listOfUsers'
    tsPts <- getTsAndPts hVK' hLogger' vktoken'
    case tsPts of
        Just (ts, pts) -> do
            threadDelay 3000000
            echo hLogger' hVK' vktoken' help_message' newListOfUsers ts pts
        Nothing -> logError hLogger' "No pts and ts parameter"
  where
    answer hLogger hVK vktoken help_message (Just (Server _ _ _ _ _ (Just messages))) xs =
        answers hLogger hVK vktoken help_message xs $ vkMessagesItems messages
    answer hLogger _ _ _ (Just _) xs = do
        logError hLogger "Unexcepted error"
        return xs
    answer hLogger _ _ _ Nothing xs = do
        logError hLogger "Unexcepted error"
        return xs
    answers hLogger hVK vktoken help_message list xs = do
        mapM_ (repeatMessage hVK hLogger vktoken list) xs
        mapM_ (sendKeyboardVk hVK hLogger vktoken) xs
        mapM_ (sendMessageHelp hVK hLogger vktoken help_message) xs
        update <- mapM (sendMessageRepeatText hVK hLogger vktoken list) xs
        return $ updateListUsers hVK list update
