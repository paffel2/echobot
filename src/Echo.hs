{-# LANGUAGE NamedFieldPuns #-}

module Echo where

import           Data.Maybe (fromJust)
import qualified UsersLists as UL

data UserMessage msg =
    UserMessage
        { from               :: UL.ChatId
        , userMessageContent :: UserMessageContent msg
        }

data UserMessageContent msg
    = CommandMessage Command
    | JustMessage msg

data Command
    = Help
    | Repeat UL.RepeatsNum
    | ChoicesRequest

data BotMessage msg =
    BotMessage
        { to                :: UL.ChatId
        , botMessageContent :: BotMessageContent msg
        }

data BotMessageContent msg
    = PlainText String
    | Keyboard
    | RepeatMessage UL.RepeatsNum msg
    | HelpMessage

data Handle msg m =
    Handle
        { getMessage           :: m (Maybe (UserMessage msg))
        , repeatsByUser        :: UL.ChatId -> m (Maybe UL.RepeatsNum)
        , updateRepeatsForUser :: UL.ChatId -> UL.RepeatsNum -> m ()
        , sendAnswer           :: BotMessage msg -> m ()
        }

echo :: Monad m => Handle msg m -> m ()
echo handler = do
    message <- getMessage handler
    let content = userMessageContent <$> message
    case content of
        Just (CommandMessage com) ->
            case com of
                Help ->
                    sendAnswer
                        handler
                        (BotMessage (from $ fromJust message) HelpMessage)
                Repeat n -> do
                    updateRepeatsForUser handler (from $ fromJust message) n
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (PlainText
                                  ("The number of repetitions is " ++
                                   show (UL.getRepeatsNum n))))
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
                             (RepeatMessage (UL.RepeatsNum 1) msg))
                Just n ->
                    sendAnswer
                        handler
                        (BotMessage
                             (from $ fromJust message)
                             (RepeatMessage n msg))
        Nothing -> return ()

data DataLoop a =
    DataLoop
        { getRepeatsList :: UL.RepeatsList
        , getUpdateId    :: Maybe a
        }
