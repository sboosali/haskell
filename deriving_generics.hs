{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Data.Aeson
import GHC.Generics
-- http://stackoverflow.com/questions/17502603/ghc-generics-or-data-data
-- http://www.haskell.org/haskellwiki/Generics


data Point = Point { x :: Double, y :: Double } deriving (Show, Generic)

instance ToJSON Point
instance FromJSON Point

-- ToJSON and FromJSON are typeclasses which implement Generic (or something like that)
-- a Generic function  is a function that works on any datatype
-- i.e. it's defined on a Generic datatype
-- i.e. it takes not the datatype's constructors, but a representation

-- the below is all false (see the haskell wiki for the truth), but it's basic idea, as i see it:
--
-- `data Point ... deriving Generic`
-- makes:
-- `instance Point Generic where`
-- ` constructors = [(Point, [x,  Number], [y, Number])]`
--
-- then:
-- `instance ToJSON Point`
-- ` toJSON ...`
-- does something like:
-- `Number $ x p`
-- `Number $ y p`
-- using `Number`and `x` and `y` from the generic `constructors`,
-- rather than directly destructing the specific `(Point x y)` and directly constructing a `Number`


main = do
 print $ encode (Point 1 2)
 print (decode "{\"x\": 1, \"y\": 2}" :: Maybe Point)
