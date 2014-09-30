import Control.Monad
import Data.List
-- http://hackage.haskell.org/package/nondeterminism-1.0/docs/Control-Monad-Amb.html
-- $ runhaskell nondeterminism.hs 

-- pythagorean triplets under some size
triplets :: Int -> [(Int, Int, Int)]
triplets n = do

 -- space to search
 a <- [1 .. n]
 b <- [1 .. n]
 c <- [1 .. n]

 -- constraints to satisfy
 guard $  a < b && b < c
 guard $  a*a + b*b == c*c
 return (a,b,c)


-- optimization: constrain search space before searching
-- still O(n^3) time
-- list monad does depth-first search
triplets' :: Int -> [(Int, Int, Int)]
triplets' n = do a <- [1       .. n]
                 b <- [(a + 1) .. n]
                 c <- [(b + 1) .. n]
                 guard $  a*a + b*b == c*c
                 return (a,b,c)

-- breadth-first search semantics via iteratively-deepening depth-first search
triplets'' :: [(Int, Int, Int)]
triplets'' = search tripletSpace'' tripletConstraints

-- with do-notation
tripletSpace'' = do
 n <- [1 .. ]
 a <- [1 .. n]
 b <- [1 .. n]
 c <- [1 .. n]
 return (a,b,c)

tripletConstraints (a,b,c) = True
 && a < b
 && b < c
 && a*a + b*b == c*c


triplets''' :: [(Int, Int, Int)]
triplets''' = search tripletSpace''' tripletConstraints

-- with list(/monad)-comprehension
tripletSpace''' = [(a,b,c) |  n <- [1 .. ],  a <- [1 .. n],  b <- [1 .. n],  c <- [1 .. n]]


-- SEARCH the SPACE for points that satisfy the CONSTRAINTS
search :: [a] -> (a -> Bool) -> [a]
search space constraints = do
 point <- space
 guard $ constraints point
 return point 


main = do
 print ""
 print $ triplets 13
 print $ triplets' 13

 print ""
 print $ takeWhile (\(_,_,c) -> c <= 13) $ nub $ triplets''
 print $ takeWhile (\(_,_,c) -> c <= 13) $ nub $ triplets'''
 -- lazily de-duplicate with nub

 print ""
 print $ take 21 $ triplets''
 print $ take 21 $ triplets'''

