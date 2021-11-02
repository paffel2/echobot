{-# LANGUAGE NamedFieldPuns #-}

module Logic where

import           Data.Maybe
import           Logger     (LogHandle)
import           UsersLists

newtype UserId =
    UserId Int

data UserMessage msg =
    UserMessage
        { from               :: ChatId
        , userMessageContent :: UserMessageContent msg
        }

data UserMessageContent msg
    = CommandMessage Command
    | JustMessage msg

data Command
    = Help
    | Repeat RepeatsNum
    | ChoicesRequest

data BotMessage msg =
    BotMessage
        { to                :: ChatId
        , botMessageContent :: BotMessageContent msg
        }

data BotMessageContent msg
    = PlainText String
    | Keyboard
    | RepeatMessage RepeatsNum msg

data Handle msg m =
    Handle
        { getMessage           :: m (Maybe (UserMessage msg))
        , repeatsByUser        :: ChatId -> m (Maybe RepeatsNum)
        , updateRepeatsForUser :: ChatId -> RepeatsNum -> m ()
        , sendAnswer           :: BotMessage msg -> m ()
        }

echo :: Monad m => String -> Handle msg m -> m ()
echo helpMessage handler = do
    message <- getMessage handler
    let content = userMessageContent <$> message
    case content of
        Just (CommandMessage com) ->
            case com of
                Help ->
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (PlainText helpMessage))
                Repeat n -> do
                    updateRepeatsForUser handler (from $ fromJust message) n
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (PlainText "Num of repeats updated"))
                ChoicesRequest ->
                    sendAnswer
                        handler
                        (BotMessage (from $ fromJust message) Keyboard)
        Just (JustMessage msg) -> do
            numOfRepeats <- repeatsByUser handler (from $ fromJust message)
            case numOfRepeats of
                Nothing ->
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (RepeatMessage (RepeatsNum 1) msg))
                Just n ->
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (RepeatMessage n msg))
        Nothing -> return ()
