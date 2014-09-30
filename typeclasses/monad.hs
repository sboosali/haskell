import Prelude hiding (liftM, sequence_, mapM_)
-- http://www.haskell.org/haskellwiki/Typeclassopedia#Monad
-- $ runhaskell monad


-- Monad Laws
-- return a >>= k  =  k a
-- m >>= return    =  m
-- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
-- fmap f xs  =  xs >>= return . f  =  liftM f xs

-- Monad Laws with (>=>)
-- return >=> g  =  g                  -- left-identity
-- g >=> return  =  g                  -- right-identity
-- (g >=> h) >=> k  =  g >=> (h >=> k) -- associativity


-- `do` notation
-- a >>= \x -> b >> c >>= \y -> d
--
-- a >>= \x ->
-- b >>
-- c >>= \y ->
-- d
--
-- do
--  x <- a
--  b
--  y <- c
--  d


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =  ma >>= (return . f)

-- apply
($$) :: Monad m => m (a -> b) -> m a -> m b
($$) mf ma = do
 f <- mf
 a <- ma
 return $ f a

-- compose
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a =  return a >>= f >>= g

-- "_" because `_ <- sequence actions` is like `sequence_ actions`
sequence_ :: Monad m => [m a] -> m () 
sequence_ =  foldr (>>) (return ())

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f =  sequence_ . map f


main = do
 -- list is a monad
 -- a list of variables/functions represents nondeterministic computation

 -- "lift" a "pure" function into a "monadic" function
 -- in the list monad, pure means deterministic, monadic means nondeterministic
 print $ liftM (+1) [1..3]

 -- given a nondeterministic variable [1..3], first multiply by 2, then add 1
 print $ [1..3] >>= (return . (*2)) >=> (return . (+1))

 -- apply a nondeterministic function to a nondeterministic variable
 print $ [(+1), (*2)] $$ [1..3]

 -- identical
 mapM_ print [1,2,3]
 sequence_ [print 1, print 2, print 3]
 print 1 >> print 2 >> print 3
 do
  print 1
  print 2
  print 3
