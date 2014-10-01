{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sorted (
 unsorted,
 sorted,
 sortedFromThenTo,
 sortedFromThen,
 sortedFromTo,
 sortedFrom,
 unsafeCoerceSorted
 -- hiding Sorted
) where

import Data.List
import Data.Maybe
import Control.Arrow
--import Test.QuickCheck

-- http://www.haskell.org/haskellwiki/Smart_constructors
-- $ runhaskell Sorted.hs


-- @Sorted@ must be sorted, by construction
-- with @newtype@, the constructor exists at compile-time, but not run-time
-- @Sorted a@ is "isomorphic" to @[a]@
newtype Sorted a = Sorted [a]
 deriving (Show, Functor)
 -- derives some @List@ typeclasses, using @List@'s dictionary

-- instance Arbitrary Sorted where
--  arbitrary = sorted arbitrary

-- eliminate @Sorted@ type
-- constructor symbol can't be used in patterns for destructing
unsorted (Sorted list) = list

-- introduce @Sorted@ type
-- sorted lists are @Sorted@
sorted :: (Ord a) => [a] -> Sorted a
sorted = sort >>> Sorted

-- enumerations are @Sorted@
sortedFromThenTo first second last = Sorted $ enumFromThenTo first second last
sortedFromThen   first second      = Sorted $ enumFromThen   first second
sortedFromTo     first        last = Sorted $ enumFromTo     first        last
sortedFrom       first             = Sorted $ enumFrom       first

-- any list may be sorted
-- checking whether sorted (linear time) is faster than sorting (super-linear time) 
sortedMaybe list
 | isSorted list = Just $ Sorted list 
 | otherwise     = Nothing

-- can this short-circuit?
isSorted list = maybe False (const True) $ foldl increasing (Just 0) list
 where
 increasing (Just old) new
  | old <= new = Just new
  | otherwise = Nothing
 increasing Nothing _ = Nothing

-- arbitrary lists may NOT be @Sorted@
-- let's user define constructors like @sortedFromThenTo@
-- not type-checked, but is "name-checked" (i.e. hard to read, easy to grep for) 
unsafeCoerceSorted = Sorted


main = do
 print $ sorted [1,3,2]
 print $ sortedFromThenTo 1 3 10

 print $ isSorted [1,3,2]
 print $ isSorted [1,2,3]
