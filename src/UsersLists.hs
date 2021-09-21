module UsersLists where

import Data.List ( find )


newtype RepeatsNum = RepeatsNum { repeats_num' :: Int} deriving (Show,Eq)
type RepeatsList = [Repeats]

data Repeats = Repeats {chat_id :: ChatId, repeats_num :: RepeatsNum} deriving (Show,Eq)
newtype ChatId = ChatId {chat_id' :: Int} deriving (Eq,Show)

findRepeatNumber :: RepeatsList -> ChatId -> RepeatsNum
findRepeatNumber listOfUsers chatId = maybe (RepeatsNum 1) repeats_num (find (\x -> chatId == chat_id x) listOfUsers)


updateListUsers :: RepeatsList -> RepeatsList -> RepeatsList
updateListUsers xs ((Repeats cid n):us) = updateListUsers newList us
  where
    newList = filter ((/= cid) . chat_id) xs ++ [Repeats cid n]
updateListUsers xs [] = xs
