{-# LANGUAGE BangPatterns #-}
import Prelude hiding (foldl, foldl')

foldl f y [] = y
foldl f y (x:xs) = foldl f (f y x) xs

foldl' f !y [] = y
foldl' f !y (x:xs) = foldl' f (f y x) xs

main = do
 print $ foldl (+) 0 [1,2,3]
 print $ foldl' (+) 0 [1..1000000] -- constant heap
 print $ foldl (+) 0 [1..1000000] -- stackoverflow

-- foldl is tail-recursive, but not strict
-- `(f y x)` is a thunk, not a value
-- each call, a new thunk is accumulated

