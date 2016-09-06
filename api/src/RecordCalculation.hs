module RecordCalculation
    ( bestAverage
    ) where

import Types

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
bestAverage n singles = DnfAverage

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
