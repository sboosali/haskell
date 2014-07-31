{-# LANGUAGE BangPatterns #-}
import Prelude hiding (foldl, foldl')
-- http://www.haskell.org/haskellwiki/Seq


foldl f y [] = y
foldl f y (x:xs) = foldl f (f y x) xs

foldl' f !y [] = y
foldl' f !y (x:xs) = foldl' f (f y x) xs

{- `seq` definition
⊥ `seq` b = ⊥
a `seq` b = b
-}

{-  equivalent
f !x !y = z

f x y
 | x `seq` y `seq` False = undefined
 | otherwise = z
-}

{- strictness violates extensionality

undefined :: a -> b
const undefined :: a -> b

extensionality means "f" equals "\x -> f x"

but [undefined] does not equal [const undefined]

TODO how?
-}


main = do
 print $ foldl (+) 0 [1,2,3]
 print $ foldl' (+) 0 [1..1000000] -- constant heap
 print $ foldl (+) 0 [1..1000000] -- stackoverflow

-- foldl is tail-recursive, but not strict
-- `(f y x)` is a thunk, not a value
-- each call, a new thunk is accumulated

