module EchoTests where

import           Control.Monad.Reader  (MonadReader (ask), ReaderT (runReaderT))
import           Control.Monad.State   (StateT, execStateT)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Map.Strict       (empty, fromList)
import           Echo                  (Command (ChoicesRequest, Help, Repeat),
                                        Handle (..), UserMessage (UserMessage),
                                        UserMessageContent (CommandMessage, JustMessage),
                                        echo)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import qualified UsersLists            as UL

testHandle ::
       Handle Int (ReaderT (UserMessage Int) (StateT (UL.DataLoop Int) Identity))
testHandle =
    Handle
        { getMessage = ask
        , repeatsByUser = UL.repeatsByUser
        , updateRepeatsForUser = UL.updateRepeatsForUser
        , sendAnswer = \_ -> return ()
        , helpMessage = "something"
        }

echoTests :: IO ()
echoTests =
    hspec $ do
        describe "Testing echo logic" $
            it "No sending message" $
            execStateT
                (mapM_ (runReaderT (echo testHandle)) [])
                (UL.DataLoop empty Nothing) `shouldBe`
            Identity (UL.DataLoop empty Nothing)
        it "  Answer to simple message" $
            execStateT
                (mapM_
                     (runReaderT (echo testHandle))
                     [UserMessage (UL.ChatId 1) (JustMessage 1)])
                (UL.DataLoop empty Nothing) `shouldBe`
            Identity (UL.DataLoop empty Nothing)
        it "  Answer to  help request" $
            execStateT
                (mapM_
                     (runReaderT (echo testHandle))
                     [UserMessage (UL.ChatId 1) (CommandMessage Help)])
                (UL.DataLoop empty Nothing) `shouldBe`
            Identity (UL.DataLoop empty Nothing)
        it "  Answer to keyboard request" $
            execStateT
                (mapM_
                     (runReaderT (echo testHandle))
                     [UserMessage (UL.ChatId 1) (CommandMessage ChoicesRequest)])
                (UL.DataLoop empty Nothing) `shouldBe`
            Identity (UL.DataLoop empty Nothing)
        it "  Confirm num of repeats changing" $
            execStateT
                (mapM_
                     (runReaderT (echo testHandle))
                     [UserMessage (UL.ChatId 1) (CommandMessage (Repeat 2))])
                (UL.DataLoop empty Nothing) `shouldBe`
            Identity
                (UL.DataLoop (fromList [(UL.ChatId 1, UL.RepeatsNum 2)]) Nothing)
