module Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_xs) = Just x
