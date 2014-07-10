{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- http://byorgey.wordpress.com/2010/03/03/deriving-pleasure-from-ghc-6-12-1/

import Data.Foldable
import Data.Monoid

data Tree a = Leaf a | Node [Tree a]
  deriving (Show, Functor, Foldable)

t :: Tree Integer
t = Node [Node [Leaf 1,
                Leaf 2],
          Node [Leaf 3,
                Leaf 4]]

treeInc = fmap (+1)

leaves = foldMap (:[])
-- [] is a Monoid, its associative binary operator is (:) under identity []

treeSum = getSum . foldMap Sum
-- Integer is no monoid, as both ((+), 0) and ((*), 1), among many other operators, work
-- in Haskell, each type can define zero or one instances of a class, not many

main = do
 print $ treeInc t
 print $ leaves t
 print $ treeSum t
