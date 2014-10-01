{-# LANGUAGE ViewPatterns #-}
module Main where
import Sorted
--import Test.QuickCheck
--import Safe

-- $ cabal install safe quickcheck
-- $ cabal exec runhaskell smart_constructors.hs


-- simulate constructor with @unsorted@ function and @ViewPatterns@ extension
-- @indices@ is an ordered list of nonnegative integers
-- splices list in one pass: supports infinite list/indices, O(n) time
byIndices list (unsorted -> indices) = indices `indexing` withIndices
 where
 withIndices = zip list [0..]

 indexing _               []                           = []
 indexing []              _                            = []
 indexing (index:indices) (seek index -> [])           = []
 indexing (index:indices) (seek index -> (first@(element,_):rest)) =
  element : indices `indexing` (first:rest)
  -- @element@ is: the only symbol in the output list; at the right place, via @seek@

 seek index list = dropUntil (\(snd -> i) -> i == index) list
 dropUntil _ []     = []
 dropUntil p (x:xs)
   | p x            = (x:xs)
   | otherwise      = dropUntil p xs

-- test :: (Positive n)[Integer] -> Sorted Integer -> Boolean
-- test list indices = (fmap (>=0) indices) ==>
--  list `byIndices` indices == filter Nothing $ map (list `atMay`) indices
-- OrderedList NonNegative Integer


main = do
 print $ [0..5] `byIndices` sorted [3,1,2]

 -- TYPE ERROR
 -- print $ [0..5] `byIndices` [3,1,2]

 -- supports indexing infinite lists by infinitely-many indices
 print $ take 3 $ [0..] `byIndices` sortedFromThen 0 2

 -- quickCheck test
