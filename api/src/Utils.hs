module Utils where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_xs) = Just x

safeRead :: Read a => String -> Maybe a
safeRead x =
  case reads x of
    [(i, _rest)] -> Just i
    _ -> Nothing
