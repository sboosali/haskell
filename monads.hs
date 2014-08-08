liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =  ma >>= (return . f)

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = do
 f <- mf
 a <- ma
 return $ f a

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a =  return a >>= f >>= g


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


main = do
 -- list is a monad
 print $ liftM (+1) [1..3]
 print $ ap [(+1), (*2)] [1..3]
