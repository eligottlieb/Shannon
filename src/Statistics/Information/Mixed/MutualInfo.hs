module Statistics.Information.Mixed.MutualInfo where

import Data.List
import Data.Matrix
import qualified Data.Vector as Vector
import qualified Statistics.Information.Continuous.Entropy as CE
import Statistics.Information.Utils.List
import Statistics.Information.Utils.Random
import System.Random

micd :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix Double -> Matrix a ->
        Double
micd g k base xs ys | nrows xs == nrows ys =
  hx - sum [ycontrib ypg | ypg <- yvals]
  where
    (g1, g2) = split g
    hx = CE.entropy g1 k base xs
    ys' = toLists ys
    yvals = probs ys' `zip` splitN (length (nub ys')) g2
    ycontrib ((y, p), g') =
      let xgiveny = [getRow i xs | i <- [1..nrows xs],
                     (Vector.toList $ getRow i ys) == y] in
        if k <= length xgiveny - 1 then
          p * CE.entropy g' k base (fromLists (map Vector.toList xgiveny))
        else
          p * hx

midc :: (RandomGen g, Eq a) => g -> Int -> Int -> Matrix a -> Matrix Double ->
        Double
midc g k base xs ys = micd g k base ys xs
