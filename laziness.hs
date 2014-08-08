import Prelude hiding ((++))


(++) :: [a] -> [a] -> [a]
[]     ++ ys =  ys
(x:xs) ++ ys =  x : (xs ++ ys)

-- `(++)` is a stream, as time/space efficient as StringBuilder


main = do
 print $ foldl (++) "" $ map (const ".") [1..10000]
