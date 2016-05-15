module Statistics.Information.Discrete.Entropy where

import Data.Matrix
import Statistics.Information.Utils.List

entropy :: Eq a => Int -> Matrix a -> Double
entropy base xs =
  -(sum (map elog . hist . toLists $ xs)) / log (fromIntegral base)
  where
    elog x | x <= 0.0 = 0
    elog x | x >= 1.0 = 0
    elog x = x * log x

centropy :: Eq a => Int -> Matrix a -> Matrix a -> Double
centropy base xs ys = entropy base (xs <|> ys) - entropy base ys
