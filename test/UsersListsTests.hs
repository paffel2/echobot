{-# LANGUAGE OverloadedStrings #-}

module UsersListsTests where

import           Control.Monad.State   (evalStateT, execStateT)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Map.Strict       (empty, fromList)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           UsersLists            (ChatId (ChatId),
                                        DataLoop (DataLoop, getRepeatsList),
                                        RepeatsNum (RepeatsNum), repeatsByUser,
                                        updateRepeatsForUser)

getNum :: Identity (Maybe RepeatsNum)
getNum = evalStateT (repeatsByUser (ChatId 5)) (DataLoop empty Nothing)

usersListsTests :: IO ()
usersListsTests =
    hspec $ do
        describe "UsersLists" $ do
            describe "repeatsByUser" $ do
                it "empty user list" $
                    evalStateT
                        (repeatsByUser (ChatId 5))
                        (DataLoop empty Nothing) `shouldBe`
                    Identity Nothing
                it "user in the list" $
                    evalStateT
                        (repeatsByUser (ChatId 5))
                        (DataLoop (fromList [(ChatId 5, RepeatsNum 2)]) Nothing) `shouldBe`
                    Identity (Just $ RepeatsNum 2)
                it "user is not in the list" $
                    evalStateT
                        (repeatsByUser (ChatId 5))
                        (DataLoop (fromList [(ChatId 4, RepeatsNum 2)]) Nothing) `shouldBe`
                    Identity Nothing
            describe "updateRepeatsForUser" $ do
                it "update empty list" $
                    getRepeatsList <$>
                    execStateT
                        (updateRepeatsForUser (ChatId 1) 5)
                        (DataLoop empty Nothing) `shouldBe`
                    Identity (fromList [(ChatId 1, RepeatsNum 5)])
                it "update number of repeats" $
                    getRepeatsList <$>
                    execStateT
                        (updateRepeatsForUser (ChatId 1) 4)
                        (DataLoop (fromList [(ChatId 1, RepeatsNum 5)]) Nothing) `shouldBe`
                    Identity (fromList [(ChatId 1, RepeatsNum 4)])
