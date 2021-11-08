{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module UsersLists where

import           Data.Aeson       (FromJSON(parseJSON) , ToJSON )
import           Data.Aeson.Types (Parser)
import           Data.Map.Strict  (Map)
import           Data.Maybe       (fromMaybe)
import           GHC.Generics     (Generic)
import           Text.Read        (readMaybe)

newtype RepeatsNum =
    RepeatsNum
        { getRepeatsNum :: Int
        }
    deriving (Show, Eq, Generic, Num)

instance FromJSON RepeatsNum where
    parseJSON v =
        RepeatsNum . fromMaybe 1 . readMaybe <$> (parseJSON v :: Parser String)

type RepeatsList = Map ChatId RepeatsNum

newtype ChatId =
    ChatId
        { getChatId :: Int
        }
    deriving (Eq, Show, Generic, Ord)
    deriving FromJSON via Int
    deriving ToJSON via Int

newtype HelpMessage =
    HelpMessage
        { getHelpMessage :: String
        }
