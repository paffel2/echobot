{-# LANGUAGE OverloadedStrings #-}

module UsersListsTests where

import Test.Hspec (describe, hspec, it, shouldBe)
import UsersLists
    

usersListsTests :: IO ()
usersListsTests =
    hspec $ do
        describe "UsersLists" $ do
            describe "findRepeatNumber" $ do
                it "empty user list" $ findRepeatNumber [] (ChatId 5) `shouldBe` (RepeatsNum 1)
                it "user in the list" $ findRepeatNumber [Repeats (ChatId 5) (RepeatsNum 3)] (ChatId 5) `shouldBe` (RepeatsNum 3)
                it "user is not in the list" $
                    findRepeatNumber [Repeats (ChatId 5) (RepeatsNum 3)] (ChatId 6) `shouldBe` (RepeatsNum 1)
            describe "updateListUsers" $ do
                it "empty update" $ updateListUsers [] [] `shouldBe` ([] :: [Repeats])
                it "update empty list" $
                    updateListUsers [] [Repeats (ChatId 5) (RepeatsNum 3)] `shouldBe` [Repeats (ChatId 5) (RepeatsNum 3)]
                it "update number of repeats" $
                    updateListUsers [Repeats (ChatId 5) (RepeatsNum 3)] [Repeats (ChatId 5) (RepeatsNum 4)] `shouldBe` [Repeats (ChatId 5) (RepeatsNum 4)]
                it "update empty list" $
                    updateListUsers [Repeats (ChatId 5) (RepeatsNum 3)] [Repeats (ChatId 4) (RepeatsNum 2)] `shouldBe`
                    [(Repeats (ChatId 5) (RepeatsNum 3)), (Repeats (ChatId 4) (RepeatsNum 2))]

