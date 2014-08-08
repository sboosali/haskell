{-# LANGUAGE ImplicitParams #-}
import Data.List (sortBy)

-- implicits

sort :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sort = sortBy ?cmp


main = do
 let ?cmp = compare
 putStrLn (show (sort [3,1,2]))
