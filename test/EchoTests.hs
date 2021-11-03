module EchoTests where

import           Control.Monad.State   (MonadState (get, put), StateT,
                                        execStateT)
import           Data.Functor.Identity (Identity (Identity))
import           Echo                  (Command (ChoicesRequest, Help, Repeat),
                                        Handle (..), UserMessage (UserMessage),
                                        UserMessageContent (CommandMessage, JustMessage),
                                        echo)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           UsersLists            (ChatId (ChatId), Repeats (Repeats),
                                        RepeatsList, RepeatsNum (RepeatsNum),
                                        findRepeatNumber, updateListUsers)

testRepeatsByUser :: ChatId -> StateT RepeatsList Identity (Maybe RepeatsNum)
testRepeatsByUser chatId = do
    someState <- get
    return $ Just $ findRepeatNumber someState chatId

testUpdateRepeatsForUser ::
       ChatId -> RepeatsNum -> StateT RepeatsList Identity ()
testUpdateRepeatsForUser chatId repeatsNum = do
    repeatsList <- get
    put $ updateListUsers repeatsList [Repeats chatId repeatsNum]

testHandle :: Handle msg (StateT RepeatsList Identity)
testHandle =
    Handle
        { getMessage = return Nothing
        , repeatsByUser = testRepeatsByUser
        , updateRepeatsForUser = testUpdateRepeatsForUser
        , sendAnswer = \_ -> return ()
        }

simpleMessage :: StateT RepeatsList Identity (Maybe (UserMessage String))
simpleMessage = return $ Just $ UserMessage (ChatId 1) (JustMessage "Something")

helpRequest :: StateT RepeatsList Identity (Maybe (UserMessage String))
helpRequest = return $ Just $ UserMessage (ChatId 1) (CommandMessage Help)

keyboardRequest :: StateT RepeatsList Identity (Maybe (UserMessage String))
keyboardRequest =
    return $ Just $ UserMessage (ChatId 1) (CommandMessage ChoicesRequest)

newRepeatsNum :: StateT RepeatsList Identity (Maybe (UserMessage String))
newRepeatsNum =
    return $
    Just $ UserMessage (ChatId 1) (CommandMessage (Repeat $ RepeatsNum 2))

echoTests :: IO ()
echoTests =
    hspec $ do
        describe "Testing echo logic" $
            it "No sending message" $
            execStateT (echo testHandle) [] `shouldBe` Identity []
        it "  Answer to simple message" $
            execStateT (echo (testHandle {getMessage = simpleMessage})) [] `shouldBe`
            Identity []
        it "  Answer to  help request" $
            execStateT (echo (testHandle {getMessage = helpRequest})) [] `shouldBe`
            Identity []
        it "  Answer to keyboard request" $
            execStateT (echo (testHandle {getMessage = keyboardRequest})) [] `shouldBe`
            Identity []
        it "  Confirm num of repeats changing" $
            execStateT (echo (testHandle {getMessage = newRepeatsNum})) [] `shouldBe`
            Identity [Repeats (ChatId 1) (RepeatsNum 2)]
