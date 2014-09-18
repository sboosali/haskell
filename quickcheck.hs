import Data.Char
import Test.QuickCheck
import Control.Applicative
-- http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck1


-- `Arbitrary` tells QuickCheck how to generate random data for some type
-- i.e. `Arbitrary a` is a random variable of `a`
instance Arbitrary Ch where
 arbitrary = fmap Ch $ choose ('\32', '\128')

newtype Ch = Ch Char deriving (Show, Eq)


-- `reverse` is polymorphic
-- QuickCheck needs to know which `Arbitrary` instances to use
reverse_isa_involution :: [Ch] -> Bool
reverse_isa_involution xs = (reverse.reverse) xs == id xs

-- (point-free and fun-filled!)
-- extensionality wrt some input
reverse_isa_involution' :: [Ch] -> Bool
reverse_isa_involution' = (reverse.reverse) === id
 where (===) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
       (===) f g = (\x -> f x == g x)


-- (point-free and fun-filled!!)
-- functions are Applicative
-- the composition operator (.) binds most tightly (infixr 9), despite appearances
-- `(reverse.reverse)` applies to the first argument and `id` applies to the second argument
reverse_isa_involution'' :: [Ch] -> Bool
reverse_isa_involution'' = (==) <$> (reverse.reverse) <*> id
-- (| (reverse.reverse) == id |) with idiom brackets


-- randomly generates 100 strings, checking each against the predicate
main = do
 quickCheck reverse_isa_involution
 quickCheck reverse_isa_involution'
 quickCheck reverse_isa_involution''
