{-# LANGUAGE OverloadedLists, TypeFamilies #-}
module Main where
import GHC.Exts
import Data.Set
import Data.Vector
import Data.Map
import Data.Text

-- https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists
-- $ runhaskell OverloadedLists.hs


instance (Ord a) => IsList (Set a) where
  type Item (Set a) = a
  fromList = Data.Set.fromList
  toList = Data.Set.toList

instance (Ord a) => IsList (Map a b) where
  type Item (Map a b) = (a, b)
  fromList = Data.Map.fromList
  toList = Data.Map.toList

newtype Natural = Natural Int deriving (Show)

-- OverloadedList literal syntax for Natural numbers
instance IsList Natural where
  type Item Natural = Int
  -- fromList is a total function, and almost smart constructor
  fromList = Natural . Prelude.length
  toList (Natural n) = [1 .. n]



main = do
 putStrLn ""

 print ([1 .. 10]                :: Vector Int)
 print (['a' .. 'z']             :: Text)
 print (['0' .. '9']             :: Set Char)
 print ([("a",0), ("b",1)]       :: Map String Int)
 print ([1 .. 10]                :: Natural)
