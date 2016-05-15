module Statistics.Information.Continuous.MutualInfo where

import Data.Matrix
import Numeric.SpecFunctions
import Statistics.Information.Utils.KNN
import Statistics.Information.Utils.Random
import System.Random

mi :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double -> Double
mi g k base xs ys | (k < nrows xs) && (nrows xs == nrows ys) =
  (-a - b + c + d) / (log $ fromIntegral base)
  where
    (g1, g2) = split g
    noisyxs = noisy g1 xs
    noisyys = noisy g2 ys
    joint = noisyxs <|> noisyys
    distances = knnDistances chebyshevDistance (toLists joint) k
    a = avgdigamma chebyshevDistance noisyxs distances
    b = avgdigamma chebyshevDistance noisyys distances
    c = digamma (fromIntegral k)
    d = digamma . fromIntegral . nrows $ xs

cmi :: RandomGen g => g -> Int -> Int -> Matrix Double -> Matrix Double ->
       Matrix Double -> Double
cmi g k base xs ys zs | (k < nrows xs) && (nrows xs == nrows ys) =
  (-a - b + c + d) / (log $ fromIntegral base)
  where
    [g1, g2, g3] = splitN 3 g
    noisyxs = noisy g1 xs
    noisyys = noisy g2 ys
    noisyzs = noisy g3 zs
    joint = noisyxs <|> noisyys <|> noisyzs
    distances = knnDistances chebyshevDistance (toLists joint) k
    a = avgdigamma chebyshevDistance (noisyxs <|> noisyzs) distances
    b = avgdigamma chebyshevDistance (noisyys <|> noisyzs) distances
    c = avgdigamma chebyshevDistance noisyzs distances
    d = digamma (fromIntegral k)
