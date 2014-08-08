import Prelude hiding (reverse)
import Control.Monad


reverse =  foldl (flip (:)) []


-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
powerset = filterM (\_ -> [True, False])


loeb x = xs where xs = fmap ($ xs) x

main = do
 print $ reverse [1..4]

 print $ powerset [1..4]

 let x = [x!!1 + 1, 2, x!!0 + x!!1]
 let l = loeb [\x -> x!!1 + 1, \x -> 2, \x -> x!!1 + x!!2]
 print $ x!!2
 print $ l!!2
