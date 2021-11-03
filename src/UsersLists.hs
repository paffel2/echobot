module UsersLists where

import           Data.Aeson       (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Aeson.Types (Parser)
import           Data.List        (find)
import           Data.Maybe       (fromMaybe)
import           GHC.Generics     (Generic)
import           Text.Read        (readMaybe)

newtype RepeatsNum =
    RepeatsNum
        { getRepeatsNum :: Int
        }
    deriving (Show, Eq, Generic)

instance FromJSON RepeatsNum where
    parseJSON v =
        RepeatsNum . fromMaybe 1 . readMaybe <$> (parseJSON v :: Parser String)

type RepeatsList = [Repeats]

data Repeats =
    Repeats
        { chat_id     :: ChatId
        , repeats_num :: RepeatsNum
        }
    deriving (Show, Eq)

newtype ChatId =
    ChatId
        { chat_id' :: Int
        }
    deriving (Eq, Show, Generic)

instance FromJSON ChatId where
    parseJSON v = ChatId <$> (parseJSON v :: Parser Int)

instance ToJSON ChatId where
    toJSON = toJSON . chat_id'

findRepeatNumber :: RepeatsList -> ChatId -> RepeatsNum
findRepeatNumber listOfUsers chatId =
    maybe
        (RepeatsNum 1)
        repeats_num
        (find (\x -> chatId == chat_id x) listOfUsers)

updateListUsers :: RepeatsList -> RepeatsList -> RepeatsList
updateListUsers xs ((Repeats cid n):us) = updateListUsers newList us
  where
    newList = filter ((/= cid) . chat_id) xs ++ [Repeats cid n]
updateListUsers xs [] = xs

newtype HelpMessage =
    HelpMessage
        { getHelpMessage :: String
        }
