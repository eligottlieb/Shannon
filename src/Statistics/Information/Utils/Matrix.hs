module Statistics.Information.Utils.Matrix where

import Data.Matrix
import qualified Data.Vector as Vector
import Statistics.Information.Utils.List

rows :: Matrix a -> [Vector.Vector a]
rows xs = [getRow i xs | i <- [1..nrows xs]]

columns :: Matrix a -> [Vector.Vector a]
columns xs = [getCol i xs | i <- [1..ncols xs]]

rowExtend :: Matrix a -> Int -> Matrix a
rowExtend xs n =
  remRows <-> Prelude.foldr (<->) xs (repeatN (completions - 1) xs) where
    completions = n `quot` nrows xs
    remRows = fromLists [Vector.toList $ getRow i xs |
                         i <- [1..n `rem` nrows xs]]
