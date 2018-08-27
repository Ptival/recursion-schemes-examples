{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Histomorphisms.Natural
  ( change
  , changeAlgebraHelper
  ) where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.List                 hiding (lookup)
import Numeric.Natural
import Prelude                   hiding (lookup)

import Algebras.CVAlgebra

-- | Computing all the ways to make change given some denominations

changeAlgebraHelper :: [Natural] -> Natural -> (Natural -> [[Natural]]) -> [[Natural]]
changeAlgebraHelper denominations currentInput lookupResult =
  let
    validDenominations  = filter (<= currentInput) denominations
    remainders          = map (\ coin -> (coin, currentInput - coin)) validDenominations
    (zeroes, toProcess) = partition (\ p -> snd p == 0) remainders
    processed           = map process toProcess
  in
  map ((: []) . fst) zeroes ++ concat processed
  where
    process :: (Natural, Natural) -> [[Natural]]
    process (coin, remainder) =
      let
        -- 0 is (currentInput - 1), 1 is (currentInput - 2), ...
        distanceInHistory = currentInput - 1 - remainder
        result            = map (coin :) . filter (all (<= coin)) $ lookupResult distanceInHistory
      in
      result -- `deepseq` result

changeAlgebra :: [Natural] -> CVAlgebra (Base Natural) [[Natural]]
changeAlgebra denominations = \case
  Nothing -> [[]]
  current@(Just history) -> changeAlgebraHelper denominations (compress current) (lookup history)
    where
      compress :: Maybe (Cofree Maybe a) -> Natural
      compress Nothing         = 0
      compress (Just (_ :< x)) = 1 + compress x

      lookup :: Cofree Maybe a -> Natural -> a
      lookup (v :< _) 0 = v
      lookup (_ :< r) n = lookup inner (n - 1) where (Just inner) = r

change :: [Natural] -> Natural -> [[Natural]]
change denominations = histo (changeAlgebra denominations)
