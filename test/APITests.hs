{-# LANGUAGE OverloadedStrings #-}

module APITests where

import Test.Hspec (describe, hspec, it, shouldBe)
import Vk.API (findRepeatNumber, updateListUsers)

apiTests :: IO ()
apiTests =
    hspec $ do
        describe "API" $ do
            describe "findRepeatNumber" $ do
                it "empty user list" $ findRepeatNumber [] 5 `shouldBe` 1
                it "user in the list" $ findRepeatNumber [(5, 3)] 5 `shouldBe` 3
                it "user is not in the list" $
                    findRepeatNumber [(5, 3)] 6 `shouldBe` 1
            describe "updateListUsers" $ do
                it "empty update" $ updateListUsers [] [] `shouldBe` []
                it "Nothing update" $ updateListUsers [] [Nothing] `shouldBe` []
                it "update empty list" $
                    updateListUsers [] [Just (5, 3)] `shouldBe` [(5, 3)]
                it "update number of repeats" $
                    updateListUsers [(5, 3)] [Just (5, 4)] `shouldBe` [(5, 4)]
                it "update empty list" $
                    updateListUsers [(5, 3)] [Just (4, 3)] `shouldBe`
                    [(5, 3), (4, 3)]
