module Vk.Echo where

import Control.Concurrent (threadDelay)
import Logger (Handle, logError)
import Vk.API (answer, getLongPollHistory, getTsAndPts)
import Vk.BuildRequests (VkToken)

echo :: Handle -> VkToken -> String -> [(Int, Int)] -> Int -> Int -> IO ()
echo hLogger vktoken help_message listOfUsers ts pts = do
    updates <- getLongPollHistory hLogger vktoken ts pts
    newListOfUsers <- answer hLogger vktoken help_message updates listOfUsers
    tsPts <- getTsAndPts hLogger vktoken
    case tsPts of
        Just (ts', pts') -> do
            threadDelay 3000000
            echo hLogger vktoken help_message newListOfUsers ts' pts'
        Nothing -> logError hLogger "No pts and ts parameter"
