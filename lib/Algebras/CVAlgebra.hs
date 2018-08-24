-- |

module Algebras.CVAlgebra
  ( CVAlgebra
  ) where

import Control.Comonad.Cofree

type CVAlgebra f a = f (Cofree f a) -> a
