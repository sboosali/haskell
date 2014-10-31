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

-- instance Default' V1 where
--  no value implies no default value

-- no fields to default
instance Default' U1 where
  def' = U1
-- default the field
instance (Default a) => Default' (K1 R a) where
  def' = K1 def
-- default each field
instance (Default' f, Default' g) => Default' (f :*: g) where
  def' = def' :*: def'

instance (Default' f) => Default' (M1 S s f) where
  def' = M1 def'
instance (Default' f) => Default' (M1 C c f) where
  def' = M1 def'
-- pick the first constructor, defaulting recursively
instance (Default' f) => Default' (f :+: g) where
  def' = L1 def' -- order is guaranteed, nesting is not guaranteed
instance (Default' f) => Default' (M1 D t f) where
  def' = M1 def'


class Default' f where
  def' :: f a -- the leftmost term is the first constructor


class Default a where
  def :: a

  default def :: (Generic a, Default' (Rep a)) => a
  def = to (def' :: Rep a x)


data X = A Integer String | B  deriving (Generic, Show)
instance Default X
-- "Default" is not a derivable class

instance Default Integer where def = 0
instance Default String

data Y = Y  deriving (Generic, Show)
instance Default Y

-- data N = S N | Z  deriving (Generic, Show) -- non-terminating at run-time 
data N = Z | S N  deriving (Generic, Show)
instance Default N


{- -}

instance Constructors' U1 where
  constructors' = [U1]
instance Constructors' (K1 R a) where
  constructors' = [K1 undefined]
-- | both 'f' and 'g' can only be an M1 with either: (:*:) or K1 or U1.
-- which all produce singleton lists: non-empty means 'head' is safe; non-"plural" singleton means no constructor is lost.
instance (Constructors' f, Constructors' g) => Constructors' (f :*: g) where
  constructors' = [head constructors' :*: head constructors']

-- | an empty type has no constructors (i.e. the length of the constructors list is zero)
instance Constructors' V1 where
  constructors' = []
-- | a sum type adds constructors (i.e. increasing the length of the constructors list)
instance (Constructors' f, Constructors' g) => Constructors' (f :+: g) where
  constructors' = map L1 constructors' ++ map R1 constructors'
-- | the M1 instances are all 'map's, preserving the length of the constructors list.
instance (Constructors' f) => Constructors' (M1 tag name f) where
  constructors' = map M1 constructors'


-- | really "values, one per constructor" not "constructors"
class Constructors' f where
 constructors' :: [f x]

class Constructors a where
 constructors :: [a]

 default constructors :: (Generic a, Constructors' (Rep a)) => [a]
 constructors = map to (constructors' :: [Rep a x])


data Empty  deriving (Generic)
instance Constructors Empty

newtype Natural = Natural Integer  deriving (Generic, Show)
instance Constructors Natural

data Z = D | E String Integer Bool | F Z  deriving (Generic, Show)
instance Constructors Z

nameNatural (Natural {}) = "Natural"

nameZ (D {}) = "D"
nameZ (E {}) = "E"
nameZ (F {}) = "F"


main = do

 putStrLn ""
 print $ encode (Node (Node Leaf 1 Leaf) 2 Leaf :: Tree Int)

 putStrLn ""
 print $ (def :: X)
 print $ (def :: Y)
 print $ (def :: N)

 putStrLn ""
 print $ length (constructors :: [Empty]) 
 print $ map nameNatural (constructors :: [Natural])
 print $ map nameZ (constructors :: [Z])
