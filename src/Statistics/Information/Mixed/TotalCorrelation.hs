module Statistics.Information.Mixed.TotalCorrelation where

import Data.Matrix
import qualified Statistics.Information.Continuous.Entropy as CE
import Statistics.Information.Continuous.MutualInfo
import qualified Statistics.Information.Continuous.TotalCorrelation as CTC
import qualified Statistics.Information.Discrete.Entropy as DE
import qualified Statistics.Information.Discrete.TotalCorrelation as DTC
import Statistics.Information.Mixed.Entropy
import Statistics.Information.Mixed.MutualInfo
import Statistics.Information.Utils.Matrix
import Statistics.Information.Utils.Random
import System.Random

ctcdc :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a -> Matrix Double ->
         Double
ctcdc g k base xs ys = sum hxs - hx where
  hxs = [centropydc gcol k base col ys | (col, gcol) <- cols]
  hx = centropydc g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)

ctccd :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix Double -> Matrix a ->
         Double
ctccd g k base xs ys = sum hxs - hx where
  hxs = [centropycd gcol k base col ys | (col, gcol) <- cols]
  hx = centropycd g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)

corexcd_tcs :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix Double ->
               Matrix a -> Double
corexcd_tcs g k base xs ys = CTC.tc g1 k base xs - ctccd g2 k base xs ys where
  (g1, g2) = split g

corexcd_mis :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix Double ->
               Matrix a -> Double
corexcd_mis g k base xs ys = sum mixs - mi_all where
  mixs = [micd gcol k base col ys | (col, gcol) <- cols]
  mi_all = micd g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)

corexdc_tcs :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a ->
               Matrix Double -> Double
corexdc_tcs g k base xs ys = DTC.tc base xs - ctcdc g k base xs ys

corexdc_mis :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a ->
               Matrix Double -> Double
corexdc_mis g k base xs ys = sum mixs - mi_all where
  mixs = [midc gcol k base col ys | (col, gcol) <- cols]
  mi_all = midc g2 k base xs ys
  (g1, g2) = split g
  cols = (map colVector (columns xs)) `zip` (splitN (ncols xs) g1)
