{-# LANGUAGE RecordWildCards #-}

module RecordCalculation
    ( bestAverage
    , newRecord
    , calculateRecord
    ) where

import Types
import Data.List ((\\), foldl')
import Control.Applicative ((<|>))

singleToMaybeTime :: Single -> Maybe DurationInMs
singleToMaybeTime s@Single{..}
    | isDnf s = Nothing
    | otherwise = Just singleTime

isBetter :: (Ord a) => Maybe a -> Maybe a -> Maybe a
isBetter (Just x) (Just y)
    | x < y = Just x
    | otherwise = Nothing
isBetter (Just x) Nothing = Just x
isBetter Nothing (Just _) = Nothing
isBetter Nothing Nothing = Nothing

-- TODO: Requires: length singles > 0
-- TODO: Is probably very slow because of `windows`
calculateRecord :: [Single] -> RecordType -> Maybe (DurationInMs, [Single])
calculateRecord singles recordType =
    case recordType of
        SingleRecord ->
            let minSingle = minimum singles
            in
                if isDnf minSingle then
                    Nothing
                else
                    Just (singleTime minSingle, [minSingle])
        _ ->
            let chunks = windows (singleCount recordType) singles
            in
                foldl' (\lastRecord s -> (fmap (\r -> (r, s)) $ newRecord s recordType (fst <$> lastRecord)) <|> lastRecord) Nothing chunks

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows n xs@(_:rest)
    | length xs < n = []
    | otherwise = take n xs : windows n rest


-- TODO: Refactor
-- TODO: Don't pass in entire Single, but (SingleResult = Success Int | Dnf), most attributes useless (could also save memory)
newRecord :: [Single] -> RecordType -> Maybe DurationInMs -> Maybe DurationInMs
newRecord mostRecentSingles type' currentRecord
    | length mostRecentSingles /= singleCount type' = Nothing
    | length mostRecentSingles == 1 && type' == SingleRecord = isBetter (singleToMaybeTime $ head mostRecentSingles) currentRecord
    | otherwise =
        case cubingAverage mostRecentSingles of
            Nothing ->
                Nothing
            Just r ->
                let
                    roundedTime = round r
                in
                    case currentRecord of
                        Just recordTime ->
                            if roundedTime < recordTime then
                                Just roundedTime
                            else
                                Nothing
                        Nothing ->
                            Just roundedTime

cubingAverage :: [Single] -> Maybe Double
cubingAverage singles
    | length singles < 3 = Nothing
    | length (filter isDnf singles) > 1 = Nothing
    | otherwise = Just $ mean $ singles \\ [fastestSingle, slowestSingle]
  where
    fastestSingle = minimum singles
    slowestSingle = maximum singles
    mean times = (fromIntegral $ sum $ singleTime <$> times) / (fromIntegral $ length times)


--data Single      = Dnf | Plus2 Int | Solved Int
data AverageTime = DnfAverage | ValidAverage Float
-- TODO use dependent types (type level natural numbers for window size)
-- data AverageOf n = AverageOf [Single]

bestAverage :: Int -> [Single] -> AverageTime
bestAverage 1 singles = let minSingle = minimum singles
                        in
                          if isDnf minSingle then
                            DnfAverage
                          else
                            ValidAverage $ fromIntegral $ singleTime minSingle
bestAverage _ _ = DnfAverage

{-
averageTime :: AverageOf n -> AverageTime
averageTime (AverageOf xs) = case sum of
                               DnfAverage -> DnfAverage
                               ValidAverage x -> ValidAverage $ (toRational x) / (toRational $ length xs)
  where
    sum = foldl (\acc x -> DnfAverage) (firstTime $ head xs) xs
    firstTime Dnf = DnfAverage
    firstTime (Plus2 x) = ValidAverage x
    firstTime (Solved x) = ValidAverage x
-}
