module Telegram.Echo where

import Logger (Handle)
import Telegram.API (answers, getLastUpdateId, getUpdates, updateListUsers)
import Telegram.BuildRequest (TelegramToken)

echo :: Handle -> TelegramToken -> Maybe Int -> String -> [(Int, Int)] -> IO ()
echo hLogger tgtoken updateId help_message listOfUsers = do
    updates <- getUpdates hLogger tgtoken updateId
    b <- answers hLogger help_message tgtoken updates listOfUsers
    let newlistOfUsers = updateListUsers listOfUsers b
    nextUpdateID <- getLastUpdateId hLogger updates
    echo hLogger tgtoken nextUpdateID help_message newlistOfUsers
