{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Text (Text, pack)
import Text.Printf (printf)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_xs) = Just x

safeRead :: Read a => String -> Maybe a
safeRead x =
  case reads x of
    [(i, _rest)] -> Just i
    _ -> Nothing

formatTime :: Int -> Text
formatTime time
    | (round $ (((fromIntegral time) / 1000) :: Float)) < 60 = let s = ((fromIntegral time) / 1000)
                     in  pack $ printf "%.2fs" (s :: Float)
    | otherwise    = let hs = round $ ((fromIntegral time) / 10) :: Int
                         min = floor $ (fromIntegral hs) / 6000 :: Int
                         s' = fromIntegral (hs - (fromIntegral $ min * 6000)) / 100
                     in  pack $ printf "%d:%05.2fmin" min (s' :: Float)
