module UsersLists where

import Data.List ( find )


type RepeatsNum = Int
type RepeatsList = [Repeats]

data Repeats = Repeats {chat_id :: ChatId, repeats_num :: RepeatsNum} deriving (Show,Eq)
type ChatId = Int

findRepeatNumber :: RepeatsList -> ChatId -> RepeatsNum
findRepeatNumber listOfUsers chatId = maybe 1 repeats_num (find (\x -> chatId == chat_id x) listOfUsers)


updateListUsers :: RepeatsList -> RepeatsList -> RepeatsList
updateListUsers xs ((Repeats cid n):us) = updateListUsers newList us
  where
    newList = filter ((/= cid) . chat_id) xs ++ [Repeats cid n]
updateListUsers xs [] = xs
