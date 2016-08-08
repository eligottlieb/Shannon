module Statistics.Information.Mixed.Entropy where

import Data.Matrix
import qualified Statistics.Information.Continuous.Entropy as CE
import qualified Statistics.Information.Discrete.Entropy as DE
import Statistics.Information.Mixed.MutualInfo
import Statistics.Information.Utils.Random
import System.Random

centropydc :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a ->
              Matrix Double -> Double
centropydc g k base xs ys = DE.entropy base xs - midc g k base xs ys

centropycd :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix Double ->
              Matrix a -> Double
centropycd g k base xs ys = CE.entropy g1 k base xs - micd g2 k base xs ys where
  (g1, g2) = split g

entropy :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a ->
           Matrix Double -> Double
entropy g k base xs ys = hx + hy - ixy
  where
    (g1, g2) = split g
    hx = DE.entropy base xs
    hy = CE.entropy g1 k base ys
    ixy = midc g2 k base xs ys