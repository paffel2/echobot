module UsersLists where

import Data.Maybe (fromMaybe)

type RepeatsNum = Int

type RepeatsList = [(ChatId, RepeatsNum)]

type ChatId = Int

findRepeatNumber :: RepeatsList -> ChatId -> RepeatsNum
findRepeatNumber listOfUsers userId = fromMaybe 1 $ lookup userId listOfUsers

updateListUsers :: RepeatsList -> [(ChatId, RepeatsNum)] -> RepeatsList
updateListUsers xs ((cid, n):us) = updateListUsers newList us
  where
    newList = filter ((/= cid) . fst) xs ++ [(cid, n)]
updateListUsers xs [] = xs
