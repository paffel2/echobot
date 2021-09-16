module Vk.Echo where


import Logger (Handle, logError)
import Vk.Types 
import Vk.Responses (VkMessages(vkMessagesItems), VkResponseType(Server))
import Vk.VkHandle
    ( VKHandle(getLongPollHistory, getTsAndPts, repeatMessage,
         sendKeyboardVk, sendMessageHelp, sendMessageRepeatText,
         updateListUsers)
    )

echo :: Monad m =>
    Handle m
    -> VKHandle m
    -> VkToken
    -> HelpMessage
    -> RepeatsList
    -> Ts
    -> Pts
    -> m (Maybe ((Ts, Pts), RepeatsList))
echo hLogger' hVK' vktoken' help_message' listOfUsers' ts' pts' = do
    updates <- getLongPollHistory hVK' hLogger' vktoken' ts' pts'
    newListOfUsers <-
        answer hLogger' hVK' vktoken' help_message' updates listOfUsers'
    tsPts <- getTsAndPts hVK' hLogger' vktoken'
    case tsPts of
        Just a -> do
            return $ Just (a,newListOfUsers)
        Nothing -> do
            logError hLogger' "No pts and ts parameter"
            return Nothing

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


