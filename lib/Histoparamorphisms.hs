{-# LANGUAGE LambdaCase #-}

-- |

module Histoparamorphisms where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import Numeric.Natural
import Prelude                   hiding (lookup)

import Histomorphisms.Natural (changeAlgebraHelper)

histoPara :: (Corecursive t, Recursive t) => (Base t (Cofree ((,) t) a) -> a) -> t -> a
histoPara = ghisto distPara

-- | DOES NOT WORK
algebra :: [Natural] -> Maybe (Cofree ((,) Natural) [[Natural]]) -> [[Natural]]
algebra denominations = \case
  Nothing -> [[]]
  Just history ->
    changeAlgebraHelper denominations (compress history) (lookup history)
    where
      compress :: Cofree ((,) Natural) [[Natural]] -> Natural
      compress (_ :< (p, _)) = p + 1

      lookup :: Cofree ((,) Natural) [[Natural]] -> Natural -> [[Natural]]
      lookup (v :< _) 0 = v
      lookup (_ :< r) n = lookup inner (n - 1) where (_, inner) = r


change :: [Natural] -> Natural -> [[Natural]]
change denominations = histoPara (algebra denominations)
