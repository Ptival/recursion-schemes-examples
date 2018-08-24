-- | Anamorphisms building lists, and their F-co-algebras

module Anamorphisms.List
  ( range
  , rangeCoAlgebra
  ) where

import Data.Functor.Foldable

import Algebras.FCoAlgebra

rangeCoAlgebra :: FCoAlgebra (Base [Int]) Int
rangeCoAlgebra 0 = Nil
rangeCoAlgebra n = Cons n (n - 1)

range :: Int -> [Int]
range n = ana rangeCoAlgebra n
