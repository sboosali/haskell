{-# LANGUAGE ViewPatterns #-}
import Data.List
-- https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns
-- $ runhaskell view_patterns


-- view patterns
-- views can use views from arguments on the right
indexing (fst -> i) ((!! i) -> x) = x + 1 

-- view patterns
-- views can be nested
endpoints (sort ->  begin : (reverse ->  end : _)) = Just (begin, end)
endpoints _ = Nothing

-- patterns
endpoints' list@(_:_:_) = Just (begin, end)
 where (begin:rest)     = sort list
       (end  :_)        = reverse rest
endpoints' _ = Nothing

-- clauses
endpoints'' list
 | length list >= 2  = Just ((head . sort) list, (head . reverse . sort) list)
 | otherwise         = Nothing

-- Python
endpoints''' list = if length list >= 2
                    then let sorted = sort list
                             begin = head sorted
                             end = head (reverse sorted)
                         in (begin, end)  
                    else undefined

quicksort _   []     = []
quicksort key (y:ys) = xs ++ [y] ++ zs
 where
 xs = quicksort key $ filter below ys
 zs = quicksort key $ filter above ys
 below x = x `cmp` y
 above = not . below
 cmp (key -> a) (key -> b) = a < b


main = do
 print $ indexing (1,2) [1,2,3]
 print $ endpoints [3,2,1,6,5,4]
 print $ endpoints' [3,2,1,6,5,4]
 print $ endpoints'' [3,2,1,6,5,4]
 print $ endpoints''' [3,2,1,6,5,4]
 print $ quicksort (* (-1))  [3,3,2,1,6,5,4]
