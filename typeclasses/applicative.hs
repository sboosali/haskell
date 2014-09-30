import Data.Map (fromList, Map)
import Control.Applicative ((<$>), (<*>))
-- $ runhaskell applicative.hs


-- indexes a list into a map by some key
indexBy :: (Ord b) => (a -> b) -> [a] -> Map b a
indexBy f = fromList . map ((,) <$> f <*> id)
-- with idiom brackets: [| (f, id) |]

{- let's eval it...

((,) <$> f <*> id)

 f <$> g = pure f <*> g

(pure (,) <*> f <*> id)

 infixl 4 <*>

((pure (,) <*> f)  <*>  id)

 instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)

 it's SKI!!! http://en.wikipedia.org/wiki/SKI_combinator_calculus
  pure  = K
  (<*>) = S

((const (,) <*> f)  <*>  id)
((\x -> const (,) x $ f x)  <*>  id)
(\x -> (\x -> const (,) x $ f x) x $ id x)

 more β reduction

(\x -> const (,) x $ f x $ id x)
(\x -> (,) $ f x $ id x)
(\x -> (,) $ f x $ x)

 syntax sugar

(\x -> (f x, x))

-}

indexBy' f = fromList . map (\x -> (f x, x))

indexBy'' f xs = fromList $ zip (map f xs) xs


{-

 class Functor f => Applicative f where
  pure :: a -> f a Source
  (<*>) :: f (a -> b) -> f a -> f b 

 instance Applicative ((->) a) where
  pure = const
  (<*>) f g x = f x (g x)


(<*>) :: f (b -> c) -> f b -> f c

 f = ((->) a)

(<*>) :: ((->) a) (b -> c)  ->  ((->) a) b  ->  ((->) a) c
(<*>) :: ((->) a (b -> c))  ->  ((->) a b)  ->  ((->) a c)

 syntax sugar

(<*>) :: (a -> (b -> c))  ->  (a -> b)  ->  (a -> c)

 right associativity

(<*>) :: (a -> b -> c)  ->  (a -> b)  ->  a  ->  c

:: (a -> b -> c)  a  (a -> b)  a
=  f              x  (g        x)

:: (a -> b -> c)  a  b
=  f              x  (g x)

:: c
=  f x (g x)


-}


data Person = Person {
 firstName :: String,
 lastName :: String
} deriving (Show)


main = do
 let me = Person "Spiros" "Boosalis"
 let he = Person "Leonard" "Cohen"
 print $ indexBy lastName [me, he]
 print $ indexBy' lastName [me, he]
 print $ indexBy'' lastName [me, he]
