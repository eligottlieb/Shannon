module Statistics.Information.Discrete.MutualInfo where

import Data.Matrix
import Statistics.Information.Discrete.Entropy

mi :: Eq a => Int -> Matrix a -> Matrix a -> Double
mi base xs ys = entropy base xs + entropy base ys - entropy base (xs <|> ys)

cmi :: Eq a => Int -> Matrix a -> Matrix a -> Matrix a -> Double
cmi base xs ys zs = hxz + hyz - hxyz - hz where
  hxz = entropy base (xs <|> zs)
  hyz = entropy base (ys <|> zs)
  hxyz = entropy base (xs <|> ys <|> zs)
  hz = entropy base zs
