import Prelude hiding (reverse)
import Control.Monad


reverse =  foldl (flip (:)) []


-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
powerset = filterM (\_ -> [True, False])


-- equality under
data Point = {x :: Int, y :: Int, z :: Int}
instance (Eq Point) where
 (==) = 

main = do

 print $ reverse [1..4]

 print $ powerset [1..4]

 let x = [x!!1 + 1, 2, x!!0 + x!!1]
 print $ x!!2
