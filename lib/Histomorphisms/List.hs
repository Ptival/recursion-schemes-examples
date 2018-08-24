{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Histomorphisms.List
  ( fibonacci
  , fibonacciAlgebra
  ) where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import Numeric.Natural

import Algebras.CVAlgebra

-- | Expressing Fibonacci as a CV-algebra gives us access to the history of
-- | computations

fibonacciAlgebra :: (Num a) => CVAlgebra (Base Natural) a
fibonacciAlgebra = \case
  Nothing -> 0
  Just (prev :< Nothing) -> 1 + prev
  Just (prev :< Just (prevprev :< _)) -> prev + prevprev

fibonacci :: (Num a) => Natural -> a
fibonacci = histo $ \case
  Nothing -> 0
  Just (prev :< Nothing) -> 1 + prev
  Just (prev :< Just (prevprev :< _)) -> prev + prevprev
