{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
import Generics.Deriving
-- http://hackage.haskell.org/package/generic-deriving-1.6.3/docs/Generics-Deriving-Base.html
-- $ sudo cabal install generic-deriving


{- a function takes a Datatype -}

data A = B Int | C A
encode'' (B x) = "B" ++ show x
encode'' (C a) = "C" ++ encode'' a

-- `encode''` is a function


{- a Generic Function takes a Representation -}

-- `f` is a representation, one of: V1 (:+:) U1 K1 (:*:) M1
-- `p` is?
class Encode' f where
  encode' :: f p -> String

-- the empty datatype has no constructor, `encode'` is never called
instance Encode' V1 where
  encode' x = undefined

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
""
class Encode a where
  encode :: a -> String

  default encode :: (Generic a, Encode' (Rep a)) => a -> String
  encode = encode' . from

-- `encode` is a generic function


{- module Generics.Deriving.Base

V1 instance -> datatype with 0 constructors

-> datatype with 1 constructor

(:+:) instance -> datatype with >1 constructors


U1 instance -> datatype with constructors with 0 fields

K1 instance -> datatype with constructors with 1 field

(:*:) instance -> datatype with constructors with >1 field


M1 instance -> any datatype

-}


data Tree a = Leaf | Node (Tree a) a (Tree a)
 deriving Generic

-- automatically implements Encode
-- uses `default encode`
instance (Encode a) => Encode (Tree a)

-- you can still manually implement Encode, without having to implement Generic
-- uses `encode`
instance Encode Int where
 encode = show


main = do
 print $ encode (Node (Node Leaf 1 Leaf) 2 Leaf :: Tree Int)
