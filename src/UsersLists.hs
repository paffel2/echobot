{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module UsersLists where

import           Control.Monad.State (MonadState, gets, modify)
import           Data.Aeson          (FromJSON (parseJSON), ToJSON)
import           Data.Aeson.Types    (Parser)
import           Data.Map.Strict     (Map, insert, lookup)
import           Data.Maybe          (fromMaybe)
import           GHC.Generics        (Generic)
import           Text.Read           (readMaybe)

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

data DataLoop a =
    DataLoop
        { getRepeatsList :: RepeatsList
        , getUpdateId    :: Maybe a
        } deriving (Show, Eq)

repeatsByUser :: MonadState (DataLoop a) m => ChatId -> m (Maybe RepeatsNum)
repeatsByUser chatId = gets (Data.Map.Strict.lookup chatId . getRepeatsList)

updateRepeatsForUser ::
       MonadState (DataLoop a) m => ChatId -> RepeatsNum -> m ()
updateRepeatsForUser chatId repeatsNums =
    modify updateDataLoopList
  where
    updateDataLoopList (DataLoop list updateId) =
        DataLoop (insert chatId repeatsNums list) updateId
