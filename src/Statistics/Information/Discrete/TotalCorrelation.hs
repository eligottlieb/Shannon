module Statistics.Information.Discrete.TotalCorrelation where

import Data.Matrix
import Statistics.Information.Discrete.Entropy
import Statistics.Information.Discrete.MutualInfo
import Statistics.Information.Utils.Matrix

tc :: Eq a => Int -> Matrix a -> Double
tc base xs = sum hxs - hx where
  hxs = [entropy base col | col <- map colVector (columns xs)]
  hx = entropy base xs

ctc :: Eq a => Int -> Matrix a -> Matrix a -> Double
ctc base xs ys = sum hxs - hx where
  hxs = [centropy base col ys | col <- map colVector (columns xs)]
  hx = centropy base xs ys

corex_tcs :: Eq a => Int -> Matrix a -> Matrix a -> Double
corex_tcs base xs ys = tc base xs - ctc base xs ys

corex_mis :: Eq a => Int -> Matrix a -> Matrix a -> Double
corex_mis base xs ys = sum mixs - mi_all where
  mixs = [mi base col ys | col <- map colVector (columns xs)]
  mi_all = mi base xs ys

residual :: Eq a => Int -> Matrix a -> Double
residual base xs = sum $ [hlessxi i | i <- [1..(ncols xs)]] where
  hlessxi i = centropy base (colVector $ getCol i xs) $ residualMatrix xs i

dtc :: Eq a => Int -> Matrix a -> Double
dtc base xs = entropy base xs - residual base xs
