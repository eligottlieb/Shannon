module Statistics.Information.Utils.KNN where

import qualified Data.KdTree.Static as KdTree
import Data.Matrix
import Data.Metric.Class
import Data.Metric.Vector.Real
import qualified Data.Vector as Vector
import Numeric.SpecFunctions
import Statistics.Information.Utils.Matrix

chebyshevDistance u v =
  distance (Chebyshev $ Vector.fromList u) (Chebyshev $ Vector.fromList v)

kdTree metric xs = KdTree.buildWithDist id metric xs

kdDistances tree metric xs k =
  map
    (\row -> let neighbor = KdTree.kNearest tree (k+1) row !! k in
      metric neighbor row)
    xs

knnDistances :: Real a => ([a] -> [a] -> a) -> [[a]] -> Int -> [a]
knnDistances metric xs k = kdDistances tree metric xs k where
  tree = kdTree metric xs

avgdigamma metric xs distances =
  mean . map (digamma . fromIntegral . length) $ radiusPoints
  where
    tree = kdTree metric (toLists xs)
    mean xs = sum xs / (fromIntegral $ length xs)
    radiusPoints = [KdTree.inRadius tree (d - 1e-15) x | (x, d) <- pds]
    pds = (map Vector.toList $ rows xs) `zip` distances
