{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec where

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = do
    describe "hashPassword" $ do
        it "works" $ do
            hashPassword (Salt "xS2D5Mty") (ClearPassword "password123") `shouldBe` (HashedPassword "560a8ac8a1290f8525669d93556226cf115cdad92236f16ca0158dae2cd14d8e")
    describe "gravatarHash" $ do
        it "works" $ do
            gravatarHash "foo@bar.com" `shouldBe` "f3ada405ce890b6f8204094deb12d8a8"
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
    describe "humanized time interval" $ do
        context "exactly 10 days" $ do
            it "displays '10 days'" $
                humanizeTimeInterval (daysToMs 10) `shouldBe` "10 days"
        context "slightly more than 10 days" $ do
            it "displays '10 days'" $
                humanizeTimeInterval (daysToMs 10.1) `shouldBe` "10 days"
        context "almost a day" $ do
            it "displays '1 day'" $
                humanizeTimeInterval (daysToMs 0.8) `shouldBe` "1 day"
        context "about three hours" $ do
            it "displays '3 hours'" $
                humanizeTimeInterval (hoursToMs 3.2) `shouldBe` "3 hours"
        context "about an hour" $ do
            it "displays '1 hour'" $
                humanizeTimeInterval (hoursToMs 0.9) `shouldBe` "1 hour"
        context "less than an hour" $ do
            it "displays 'less than 1 hour'" $
                humanizeTimeInterval (hoursToMs 0.4) `shouldBe` "less than an hour"

daysToMs d = round $ d * 24 * 60 * 60 * 1000
hoursToMs h = round $ h * 60 * 60 * 1000
