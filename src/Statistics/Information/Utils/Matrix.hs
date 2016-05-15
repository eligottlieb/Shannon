module Statistics.Information.Utils.Matrix where

import Data.Matrix
import Data.Vector

rows :: Matrix a -> [Vector a]
rows xs = [getRow i xs | i <- [1..nrows xs]]

columns :: Matrix a -> [Vector a]
columns xs = [getCol i xs | i <- [1..ncols xs]]
