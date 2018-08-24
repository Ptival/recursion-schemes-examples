{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |

module DataTypes
  ( Tree(..)
  , TreeF(..)
  )where

import Data.Functor.Foldable.TH

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)

makeBaseFunctor ''Tree
