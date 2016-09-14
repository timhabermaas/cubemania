{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

import Test.Hspec
import Utils

spec :: Spec
spec = do
    describe "formatTime" $ do
        context "when time is less than a minute" $ do
            it "displays the time in seconds" $
                formatTime 13373 `shouldBe` "13.37s"
            it "rounds to the next 1/100th second" $
                formatTime 13375 `shouldBe` "13.38s"
        context "when time is a minute or more" $ do
            it "displays the time in minutes" $
                formatTime 60000 `shouldBe` "1:00.00min"
            it "rounds to the next 1/100th second" $
                formatTime 70507 `shouldBe` "1:10.51min"
        context "when time is right before a minute" $ do
            it "rounds to a minute" $ do
                formatTime 59999 `shouldBe` "1:00.00min"
