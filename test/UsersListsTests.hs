{-# LANGUAGE OverloadedStrings #-}

module UsersListsTests where

import Test.Hspec (describe, hspec, it, shouldBe)
import UsersLists
    ( findRepeatNumber, updateListUsers, Repeats(Repeats) )

usersListsTests :: IO ()
usersListsTests =
    hspec $ do
        describe "UsersLists" $ do
            describe "findRepeatNumber" $ do
                it "empty user list" $ findRepeatNumber [] 5 `shouldBe` 1
                it "user in the list" $ findRepeatNumber [Repeats 5 3] 5 `shouldBe` 3
                it "user is not in the list" $
                    findRepeatNumber [Repeats 5 3] 6 `shouldBe` 1
            describe "updateListUsers" $ do
                it "empty update" $ updateListUsers [] [] `shouldBe` ([] :: [Repeats])
                it "update empty list" $
                    updateListUsers [] [Repeats 5 3] `shouldBe` [Repeats 5 3]
                it "update number of repeats" $
                    updateListUsers [Repeats 5 3] [Repeats 5 4] `shouldBe` [Repeats 5 4]
                it "update empty list" $
                    updateListUsers [Repeats 5 3] [Repeats 4 3] `shouldBe`
                    [Repeats 5 3, Repeats 4 3]
