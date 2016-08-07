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
  if remainder > 0 then
    remRows <-> Prelude.foldr (<->) xs (repeatN (completions - 1) xs)
  else
    Prelude.foldr (<->) xs (repeatN (completions - 1) xs)
  where
    completions = n `quot` nrows xs
    remRows = fromLists [Vector.toList $ getRow i xs |
                         i <- [1..remainder]]
    remainder = n `rem` nrows xs

residualMatrix :: Matrix a -> Int -> Matrix a
xsiLeft xs i = submatrix 1 (nrows xs) 1 i xs
xsiRight xs i = submatrix 1 (nrows xs) (i+1) (ncols xs) xs
residualMatrix xs i | i == (ncols xs) = xsiLeft xs i
residualMatrix xs 1 = xsiRight xs 1
residualMatrix xs i = xsiLeft xs i <|> xsiRight xs i