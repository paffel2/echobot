{-# LANGUAGE OverloadedStrings #-}

module UsersListsTests where

import Test.Hspec (describe, hspec, it, shouldBe)
import UsersLists (findRepeatNumber, updateListUsers)

usersListsTests :: IO ()
usersListsTests =
    hspec $ do
        describe "UsersLists" $ do
            describe "findRepeatNumber" $ do
                it "empty user list" $ findRepeatNumber [] 5 `shouldBe` 1
                it "user in the list" $ findRepeatNumber [(5, 3)] 5 `shouldBe` 3
                it "user is not in the list" $
                    findRepeatNumber [(5, 3)] 6 `shouldBe` 1
            describe "updateListUsers" $ do
                it "empty update" $ updateListUsers [] [] `shouldBe` []
                it "update empty list" $
                    updateListUsers [] [(5, 3)] `shouldBe` [(5, 3)]
                it "update number of repeats" $
                    updateListUsers [(5, 3)] [(5, 4)] `shouldBe` [(5, 4)]
                it "update empty list" $
                    updateListUsers [(5, 3)] [(4, 3)] `shouldBe`
                    [(5, 3), (4, 3)]
