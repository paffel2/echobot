{-# LANGUAGE NamedFieldPuns #-}

module Echo where

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

data Handle msg m =
    Handle
        { getMessage           :: m (UserMessage msg)
        , repeatsByUser        :: UL.ChatId -> m (Maybe UL.RepeatsNum)
        , updateRepeatsForUser :: UL.ChatId -> UL.RepeatsNum -> m ()
        , sendAnswer           :: BotMessage msg -> m ()
        , helpMessage          :: String
        }

echo :: Monad m => Handle msg m -> m ()
echo handler = do
    message <- getMessage handler
    let content = userMessageContent message
    let send = sendAnswer handler . BotMessage (from message)
    case content of
        CommandMessage com ->
            case com of
                Help -> send (PlainText $ helpMessage handler)
                Repeat n -> do
                    updateRepeatsForUser handler (from message) n
                    send
                        (PlainText
                             ("The number of repetitions is " ++
                              show (UL.getRepeatsNum n)))
                ChoicesRequest -> send Keyboard
        JustMessage msg -> do
            numOfRepeats <- repeatsByUser handler (from message)
            case numOfRepeats of
                Nothing -> send (RepeatMessage 1 msg)
                Just n  -> send (RepeatMessage n msg)
