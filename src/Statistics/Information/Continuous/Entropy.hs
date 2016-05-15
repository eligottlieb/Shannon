module Statistics.Information.Continuous.Entropy where

import Data.Matrix
import Numeric.SpecFunctions
import Statistics.Information.Utils.KNN
import Statistics.Information.Utils.Random
import System.Random

entropy :: RandomGen g => g -> Int -> Int -> Matrix Double -> Double
entropy g k base xs | k < nrows xs =
  let noised = toLists $ noisy g xs
      distances = knnDistances chebyshevDistance noised k in
    (const + (fromIntegral d) * mean (map log distances)) /
    (log $ fromIntegral base)
  where
    d = ncols xs
    n = nrows xs
    const = digamma (fromIntegral n) - digamma (fromIntegral k) +
            (fromIntegral d) * log 2
    mean xs = sum xs / (fromIntegral $ length xs)

centropy :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double ->
            Double
centropy g k base xs ys = joint - hy where
  joint = entropy g1 k base (xs <|> ys)
  hy = entropy g2 k base ys
  (g1, g2) = split g
