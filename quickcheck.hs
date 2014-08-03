import Data.Char
import Test.QuickCheck
-- http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck1


-- `Arbitrary` tells QuickCheck how to generate random data for some type
-- i.e. `Arbitrary a` is a random variable of `a`
instance Arbitrary Ch where
 arbitrary = fmap Ch $ choose ('\32', '\128')

newtype Ch = Ch Char deriving (Show, Eq)


-- `reverse` is polymorphic
-- QuickCheck needs to know which `Arbitrary` instances to use
reverse_isa_involution :: [Ch] -> Bool
reverse_isa_involution = (reverse.reverse) === id

-- (for fun)
-- extensionality wrt some input
(===) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
(===) f g = (\x -> f x == g x)


-- randomly generates 100 strings, checking each against the predicate
main = do
 quickCheck reverse_isa_involution
