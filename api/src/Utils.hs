{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( formatTime
    , humanizeTimeInterval
    , safeRead
    , safeHead
    ) where

import Data.Text (Text, pack)
import Text.Printf (printf)
import Data.Monoid ((<>))
import Types

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_xs) = Just x

safeRead :: Read a => String -> Maybe a
safeRead x =
  case reads x of
    [(i, _rest)] -> Just i
    _ -> Nothing

formatTime :: DurationInMs -> Text
formatTime time
    | (round $ (((fromIntegral time) / 1000) :: Float)) < 60 = let s = ((fromIntegral time) / 1000)
                     in  pack $ printf "%.2fs" (s :: Float)
    | otherwise    = let hs = round $ ((fromIntegral time) / 10) :: Int
                         min = floor $ (fromIntegral hs) / 6000 :: Int
                         s' = fromIntegral (hs - (fromIntegral $ min * 6000)) / 100
                     in  pack $ printf "%d:%05.2fmin" min (s' :: Float)

msToDays t = (fromIntegral t) / (24 * 60 * 60 * 1000)
msToHours t = (fromIntegral t) / (60 * 60 * 1000)

humanizeTimeInterval :: DurationInMs -> Text
humanizeTimeInterval t
    | msToDays t >= 0.5 = pluralize (round $ msToDays t) "day"
    | msToHours t >= 0.5 = pluralize (round $ msToHours t) "hour"
    | otherwise = "less than an hour"

pluralize :: (Integral a, Show a) => a -> Text -> Text
pluralize x t = (pack $ show x) <> " " <> t <> suffix
  where
    suffix = if x > 1 then "s" else ""
