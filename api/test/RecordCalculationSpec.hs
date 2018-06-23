{-# LANGUAGE OverloadedStrings #-}

module RecordCalculationSpec where

import Test.Hspec
import RecordCalculation
import Types

import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

data DnfOrTime = NotFinished | Time DurationInMs

singleBuilder :: DnfOrTime -> Single
singleBuilder time =
    case time of
        NotFinished -> defaultSingle { singlePenalty = Just Types.Dnf }
        Time t -> defaultSingle { singleTime = t }
  where
    defaultSingle = Single
        { singleId = SingleId 2
        , singleTime = 0
        , singleComment = Nothing
        , singleScramble = "F U"
        , singlePenalty = Nothing
        , singleCreatedAt = unsafePerformIO $ getCurrentTime
        , singleUserId = UserId 2
        , singlePuzzleId = PuzzleId 2
        }

spec :: Spec
spec = do
    describe "calculateRecord" $ do
        describe "AverageOf5Record" $ do
            describe "given too few singles" $ do
                it "returns Nothing" $ do
                    -- Using replicate 0 instead of [] to avoid type annotation
                    let singles = replicate 0 (singleBuilder $ Time 10)
                    calculateRecord singles AverageOf5Record `shouldBe` Nothing
            describe "given exact 5 singles" $ do
                it "returns the average of these singles" $ do
                    let singles = replicate 5 (singleBuilder $ Time 10)
                    calculateRecord singles AverageOf5Record `shouldBe` Just (10, singles)
            describe "given 6 singles with the last two being faster" $ do
                it "returns the average of the second to last 5 singles" $ do
                    let singles = [singleBuilder $ Time 11] ++ replicate 4 (singleBuilder $ Time 10) ++ [singleBuilder $ Time 8, singleBuilder $ Time 7]
                    calculateRecord singles AverageOf5Record `shouldBe` Just (9, take 5 $ drop 2 singles)
            describe "given 6 singles with the last one being slower" $ do
                it "returns the average of the first 5 singles" $ do
                    let singles = replicate 5 (singleBuilder $ Time 10) ++ [singleBuilder NotFinished]
                    calculateRecord singles AverageOf5Record `shouldBe` Just (10, take 5 singles)
    -- TODO: More test cases
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
                let singles = singleBuilder <$> Time <$> [10, 3, 20, 30, 40]
                newRecord singles AverageOf5Record Nothing `shouldBe` Just 20
        describe "given faster average of 5" $ do
            it "returns the new record" $ do
                let singles = singleBuilder <$> Time <$> [10, 3, 20, 30, 40]
                newRecord singles AverageOf5Record (Just 21) `shouldBe` Just 20
