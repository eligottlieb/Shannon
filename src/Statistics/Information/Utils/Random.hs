module Statistics.Information.Utils.Random where

import Data.Matrix
import Statistics.Information.Utils.List
import System.Random

splitN :: RandomGen g => Int -> g -> [g]
splitN 0 _ = []
splitN 1 g = [g]
splitN n g = let (g', g'') = split g in
  g' : splitN (n-1) g''

rands :: (RandomGen g, Random a) => g -> Int -> (a, a) -> [a]
rands g n (a, b) = take n $ randomRs (a, b) g

noisy :: RandomGen g => g -> Matrix Double -> Matrix Double
noisy g xs =
  let noise = [repeatN d (intens * r) | r <- rands g n (0.0, fromIntegral n)] in
    elementwise (+) xs (fromLists noise)
  where
    intens = 1e-10
    n = nrows xs
    d = ncols xs
