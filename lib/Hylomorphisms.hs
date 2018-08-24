-- |

module Hylomorphisms where

import           Data.Functor.Foldable

import qualified Anamorphisms.List     as AL
import qualified Catamorphisms.List    as CL

-- | Given a number n, we can build the list [n, n-1, ..., 1], and then sum all
-- | those numbers together

sumNumbers :: Int -> Int
sumNumbers = hylo CL.sumAlgebra AL.rangeCoAlgebra

-- | Given a number n, we can build the list [n, n-1, ..., 1], and compute the
-- | factorial by multiplying them all together

factorial :: Int -> Int
factorial = hylo CL.productAlgebra AL.rangeCoAlgebra
