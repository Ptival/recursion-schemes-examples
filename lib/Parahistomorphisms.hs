{-# LANGUAGE LambdaCase #-}

-- |

module Parahistomorphisms where

import Control.Comonad.Cofree
import Control.Comonad.Trans.Env
import Data.Functor.Foldable
import Numeric.Natural
import Prelude                   hiding (lookup)

import Histomorphisms.Natural (changeAlgebraHelper)

paraHisto :: (Corecursive t, Recursive t) => (Base t (EnvT t (Cofree (Base t)) a) -> a) -> t -> a
paraHisto = gpara distHisto

-- | DOES NOT WORK
changeAlgebra :: [Natural] -> Maybe (EnvT Natural (Cofree Maybe) [[Natural]]) -> [[Natural]]
changeAlgebra denominations = \case
  Nothing -> [[]]
  Just history ->
    history `seq`
    changeAlgebraHelper denominations (compress history) (lookup history)
    where
      compress :: EnvT Natural (Cofree Maybe) [[Natural]] -> Natural
      compress e =
        let (p, _) = runEnvT e in
        p + 1

      lookup :: EnvT Natural (Cofree Maybe) [[Natural]] -> Natural -> [[Natural]]
      lookup e = go (snd $ runEnvT e)
        where
          go :: Cofree Maybe [[Natural]] -> Natural -> [[Natural]]
          go (s :< _)       0 = s
          go (_ :< r)  n = go inner (n - 1) where Just inner = r

changeParaHisto :: [Natural] -> Natural -> [[Natural]]
changeParaHisto denominations = paraHisto (changeAlgebra denominations)
