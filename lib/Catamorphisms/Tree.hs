-- | Catamorphisms operating on trees, and their F-algebras

module Catamorphisms.Tree
  ( countInternalNodes
  , countInternalNodesAlgebra
  , search
  , searchAlgebra
  ) where

import Data.Functor.Foldable

import Algebras.FAlgebra
import DataTypes

-- | Counting the internal nodes of a tree

countInternalNodesAlgebra :: FAlgebra (Base (Tree a)) Int
countInternalNodesAlgebra LeafF         = 0
countInternalNodesAlgebra (NodeF _ l r) = l + r

countInternalNodes :: Tree a -> Int
countInternalNodes = cata countInternalNodesAlgebra

searchAlgebra :: (Eq a) => a -> FAlgebra (Base (Tree a)) Bool
searchAlgebra _      LeafF             = False
searchAlgebra target (NodeF value l r) = target == value || l || r

search :: (Eq a) => a -> Tree a -> Bool
search a = cata (searchAlgebra a)
