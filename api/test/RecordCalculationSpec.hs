{-# LANGUAGE OverloadedStrings #-}

module RecordCalculationSpec where

import Test.Hspec
import RecordCalculation
import Types
import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

singleBuilder :: DurationInMs -> Single
singleBuilder time =
    Single
        { singleId = SingleId 2
        , singleTime = time
        , singleComment = Just ""
        , singleScramble = ""
        , singlePenalty = Nothing
        , singleCreatedAt = unsafePerformIO $ getCurrentTime
        , singleUserId = UserId 2
        , singlePuzzleId = PuzzleId 3
        }

spec :: Spec
spec = do
    describe "newRecord" $ do
        describe "given too few singles" $ do
            it "returns Nothing" $ do
                let singles = replicate 10 undefined
                newRecord singles AverageOf12Record undefined `shouldBe` Nothing
        describe "given too many singles" $ do
            it "returns Nothing" $ do
                let singles = replicate 13 undefined
                newRecord singles AverageOf12Record undefined `shouldBe` Nothing
        describe "given average without existing record" $ do
            it "returns the new record" $ do
                let singles = singleBuilder <$> [10, 3, 20, 30, 40]
                newRecord singles AverageOf5Record Nothing `shouldBe` Just 20
        describe "given faster average of 5" $ do
            it "returns the new record" $ do
                let singles = singleBuilder <$> [10, 3, 20, 30, 40]
                newRecord singles AverageOf5Record (Just 21) `shouldBe` Just 20
