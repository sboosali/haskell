{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.Aeson
import GHC.Generics
-- http://stackoverflow.com/questions/17502603/ghc-generics-or-data-data
-- http://www.haskell.org/haskellwiki/Generics


data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)

instance ToJSON Point
instance FromJSON Point

-- ToJSON and FromJSON are typeclasses which implement Generic (or something like that)
-- a Generic function is a function that works on any datatype
-- i.e. it's defined on a Generic datatype
-- i.e. it takes not the datatype's constructors, but a representation

-- the below is all false (see the haskell wiki for the truth), but it's basic idea, as i see it:
--
-- `data Point ... deriving Generic`
-- makes:
-- `instance Point Generic where`
-- ` constructors = [("Point", ["x", x, Double], ["y", y, Double])]`
--
-- then:
-- `instance ToJSON Point`
-- ` toJSON ...`
-- does something like:
-- `map (name, value -> name .= Number $ value) constructors`
-- using `Double` and `x` and `y` from the generic `constructors`,
-- rather than directly destructing the specific `(Point x y)` and directly constructing a `Double`


{- module Data.Data

-- | Representation of datatypes.
data DataType = DataType
                        { tycon   :: String
                        , datarep :: DataRep
                        }

-- | Representation of constructors.
data Constr = Constr
                        { conrep    :: ConstrRep
                        , constring :: String
                        , confields :: [String] -- for AlgRep only
                        , confixity :: Fixity   -- for AlgRep only
                        , datatype  :: DataType
                        }

-- | Public representation of datatypes
data DataRep = AlgRep [Constr]
             | IntRep
             | FloatRep
             | CharRep
             | NoRep

            deriving (Eq,Show)


-- | Public representation of constructors
data ConstrRep = AlgConstr    ConIndex
               | IntConstr    Integer
               | FloatConstr  Rational
               | CharConstr   Char

               deriving (Eq,Show)

-}

{-

class Generic a where
  -- Encode the representation of a user datatype
  type Rep a :: * -> *
  -- Convert from the datatype to its representation
  from  :: a -> (Rep a) x
  -- Convert from the representation to the datatype
  to    :: (Rep a) x -> a

-}


{-

class Encode a where
  encode :: a -> [Bit]
  default encode :: (Generic a, Encode1 (Rep a)) => a -> [Bit]
  encode = encode1 . from

data A = A Int deriving (Generic)
instance Encode A
-- default encode :: (Generic a, Encode1 (Rep a)) => a -> [Bit]

data B = B Int deriving ()
instance Encode B where
 -- encode :: a -> [Bit]
 encode (B x) = encode x

with the `default` keyword, you can still implement concrete instances, without deriving a generic representation
-}


main = do
 print $ encode (Point 1 2)
 print (decode "{\"x\": 1, \"y\": 2}" :: Maybe Point)

