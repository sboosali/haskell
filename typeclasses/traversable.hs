{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
import Data.Traversable
import Control.Applicative
import Data.Functor
import Data.Foldable
import Prelude hiding (foldr)

-- $ runhaskell traversable


{-

class (Functor t, Foldable t) => Traversable t where
 traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
 deriving (Show, Eq, Ord, Functor, Foldable)


{- (instantiating and sharing types)

Functor t
fmap :: (a -> b) -> t a -> t b

Foldable t
foldr :: (a -> b -> b) -> b -> t a -> b

Applicative f
pure  :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
infixl 4 <*>

Leaf  ::                    t a
Node  :: t a -> a -> t a -> t a

-}

instance Traversable Tree where
 -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
 traverse act Leaf = pure Leaf
  -- pure Node :: f (t b) -> (f b -> (f (t b) -> f (t b)))
 traverse act (Node left value right) = pure Node   -- pure Node :: 
  <*> traverse act left                             -- f (t b) ->
  <*> act value                                     --  (f b ->
  <*> traverse act right                            --   (f (t b) ->
                                                    --     f (t b)))
  -- in-order traversal


main = do
{-
    4
 2     6
1 3   5 7
-}
 let tree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf))

 print $ foldr (:) [] tree

 -- e.g. traverse :: (Int -> IO ()) -> Tree Int -> IO (Tree ())
 units <- traverse print tree
 print units
