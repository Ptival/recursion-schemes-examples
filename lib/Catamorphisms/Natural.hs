{-# LANGUAGE LambdaCase #-}

-- |

module Catamorphisms.Natural
  ( fibonacci
  , fibonacciAlgebra
  ) where

import Data.Functor.Foldable
import Numeric.Natural
import Prelude                  hiding (product, sum)

import Algebras.FAlgebra

-- | We can compute the Fibonacci sequence using a catamorphism, though it
-- | requires a bit of bookkeeping.  Histomorphisms let us abstract over it.

fibonacciAlgebra :: (Num a) => FAlgebra (Base Natural) (a, a)
fibonacciAlgebra = \case
  Nothing     -> (0, 1)
  Just (m, n) -> (n, m + n)

fibonacci :: (Num a) => Natural -> a
fibonacci = fst . cata fibonacciAlgebra
