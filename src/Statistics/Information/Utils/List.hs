module Statistics.Information.Utils.List where

import Data.List

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

probs :: (Eq a, Fractional b) => [a] -> [(a, b)]
probs xs = [(x, (fromIntegral $ count x xs) / len) | x <- nub xs]
  where
    len = fromIntegral $ length xs

hist :: (Eq a, Fractional b) => [a] -> [b]
hist xs = map snd (probs xs)
