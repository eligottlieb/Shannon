module Statistics.Information.Continuous.TotalCorrelation where

import Data.Matrix
import Statistics.Information.Continuous.Entropy
import Statistics.Information.Continuous.MutualInfo
import Statistics.Information.Utils.Matrix
import Statistics.Information.Utils.Random
import System.Random

tc :: RandomGen g => g -> Int -> Int -> Matrix Double -> Double
tc g k base xs = sum hxs - hx where
  hxs = [entropy gcol k base col | (col, gcol) <- cols]
  hx = entropy g2 k base xs
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)

ctc :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double ->
       Double
ctc g k base xs ys = sum hxs - hx where
  hxs = [centropy gcol k base col ys | (col, gcol) <- cols]
  hx = centropy g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)

corex_tcs :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double ->
             Double
corex_tcs g k base xs ys = tc g1 k base xs - ctc g2 k base xs ys where
  (g1, g2) = split g

corex_mis :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double ->
             Double
corex_mis g k base xs ys = sum mixs - mi_all where
  mixs = [mi gcol k base col ys | (col, gcol) <- cols]
  mi_all = mi g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)
