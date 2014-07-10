import Data.Char
import Test.QuickCheck
 
instance Arbitrary Char where
    arbitrary     = choose ('\32', '\128')
