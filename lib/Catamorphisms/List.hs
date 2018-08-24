-- |

module Catamorphisms.List
  ( product
  , productAlgebra
  , sum
  , sumAlgebra
  ) where

import Data.Functor.Foldable
import Prelude                  hiding (product, sum)

import Algebras.FAlgebra

-- | Summing all the elements of a list of numeric values

sumAlgebra :: (Num a) => FAlgebra (Base [a]) a
sumAlgebra Nil        = 0
sumAlgebra (Cons h r) = h + r

sum :: (Num a) => [a] -> a
sum = cata sumAlgebra

-- | Multiplying all the elements of a list of numeric values

productAlgebra :: (Num a) => FAlgebra (Base [a]) a
productAlgebra Nil        = 1
productAlgebra (Cons h r) = h * r

product :: (Num a) => [a] -> a
product = cata productAlgebra
