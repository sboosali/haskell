{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
import Generics.Deriving
-- http://hackage.haskell.org/package/generic-deriving-1.6.3/docs/Generics-Deriving-Base.html
-- $ cabal install generic-deriving
-- $ cabal exec runhaskell generic_programming.hs


{- a function takes a Datatype -}

-- data A = B Int | C A
-- encode'' (B x) = "B " ++ show x
-- encode'' (C a) = "C (" ++ encode'' a ++ ")"

-- `encode''` is a function


{- a Generic Function takes a Representation -}

-- `f` is a representation, one of: V1 (:+:) U1 K1 (:*:) M1
-- `p` is?
class Encode' f where
  encode' :: f p -> String

-- the empty datatype has no constructor, `encode'` is never called
instance Encode' V1 where
  encode' _ = undefined

-- mark which constructor and encode its fields
-- `f` and `g` are representations, one of: V1 (:+:) U1 K1 (:*:) M1
instance (Encode' f, Encode' g) => Encode' (f :+: g) where
  encode' (L1 x) = 'L' : encode' x
  encode' (R1 x) = 'R' : encode' x

-- the empty constructor has no fields to encode
instance Encode' U1 where
  encode' U1 = []

-- call `encode` not `encode'`
-- `i` is?
-- `c` is?
instance (Encode c) => Encode' (K1 i c) where
  encode' (K1 x) = encode x

-- add together constructor's encoded fields
-- `f` and `g` are representations, one of: V1 (:+:) U1 K1 (:*:) M1
instance (Encode' f, Encode' g) => Encode' (f :*: g) where
  encode' (x :*: y) = encode' x ++ encode' y

-- pass
-- `i` is "information" about the constructors, the datatype, the record selectors?
-- `t` is?
-- `f` is a representation, one of: V1 (:+:) U1 K1 (:*:) M1
instance (Encode' f) => Encode' (M1 i t f) where
  encode' (M1 x) = encode' x

class Encode a where
  encode :: a -> String
  -- `encode` is a function
  -- manual instances don't need a `Generic` instance

  default encode :: (Generic a, Encode' (Rep a)) => a -> String
  -- `default encode` is a generic function
  -- automatic instances use the `Generic` instance
  encode = encode' . from
  -- encode' :: (Rep r) => r -> String


{- module Generics.Deriving.Base


V1 instance -> datatype with 0 constructors

-> datatype with 1 constructor

(:+:) instance -> datatype with >1 constructors


U1 instance -> constructor with 0 fields

K1 instance -> constructor with 1 field

(:*:) instance -> constructor with >1 field


M1 instance -> any datatype

-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
 deriving Generic

-- automatically implements Encode
-- uses `default encode`
instance (Encode a) => Encode (Tree a)

-- you can still manually implement Encode, without having to implement Generic, via `default`
-- uses `encode`
instance Encode Int where
 encode = show



{- Generic Default -}
{- picks the first constructor as the default value, recurring on its arguments -}

-- non-terminating?
--  -- order is guaranteed, nesting is not guaranteed

instance Default' U1 where
  def' = U1
instance (Default a) => Default' (K1 tag a) where
  def' = K1 def -- recur into any defaultable type
instance (Default' f, Default' g) => Default' (f :*: g) where
  def' = def' :*: def'

instance (Default' f) => Default' (S1 s f) where
  def' = M1 def'
instance (Default' f) => Default' (C1 c f) where
  def' = M1 def'
instance (Default' f) => Default' (f :+: g) where
  def' = L1 def'
instance (Default' f) => Default' (D1 t f) where
  def' = M1 def'


class Default' f where
  def' :: f a -- the leftmost term is the first constructor


class Default a where
  def :: a

  -- the default Default is the first constructor, defaulting recursively
  default def :: (Generic a, Default' (Rep a)) => a
  def = to (def' :: Rep a x)


data X = A Integer String | B  deriving (Generic, Show)
instance Default X

instance Default Integer where def = 0
instance Default String

data Y = Y deriving (Generic, Show)
instance Default Y


main = do
 print $ encode (Node (Node Leaf 1 Leaf) 2 Leaf :: Tree Int)
 print $ (def :: X)
 print $ (def :: Y)
