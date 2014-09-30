import Prelude hiding (Left, Right)

-- a zipper is a data structure
-- it has a hole and a context
-- local edits are constant time (where "local" means near the hole) 
-- in the zipper list below, you can insert elements into the middle of the list, at the location of the hole, in constant time


type Zipped a = ([a], a, [a])
--              (before, here, after)

zipped :: [a] -> Zipped a
zipped [] = error "zipped: empty list"
zipped (z:zs) = ([], z, zs)

unzipped :: Zipped a -> [a]
unzipped (xs, y, zs) = foldl (flip (:)) (y:zs) xs
-- unzipped ([2,1],3,[4]) = foldl (flip (:)) [3,4] [2,1]
--                        = foldl (flip (:)) [2,3,4] [1]
--                        = foldl (flip (:)) [1,2,3,4] []
 
data Action a = Insert a | Delete | Move Direction
data Direction = Left | Right

run :: Action a -> Zipped a -> Zipped a
-- the cases are: either leftmost or rightmost

run (Insert w)   (xs,   y, zs)   = (xs,   w, y:zs) -- push the middle to the right

run Delete       (xs,   y, [])   = (xs,   y,   []) -- identity
run Delete       (xs,   y, z:zs) = (xs,   z,   zs) -- pull the right into the middle

run (Move Left)  ([],   y, zs)   = ([],   y,   zs) -- identity
run (Move Left)  (x:xs, y, zs)   = (xs,   x, y:zs)

run (Move Right) (xs,   y, [])   = (xs,   y,   []) -- identity
run (Move Right) (xs,   y, z:zs) = (y:xs, z,   zs)

runs :: [Action a] -> Zipped a -> Zipped a
runs = flip $ foldl (flip run)
-- we need foldl for the action association
-- we need flip for the argument order


main = do
 let old = zipped [1..6]
 let actions = [Move Right, Move Right, Move Right, Move Left, Delete, Insert 333, Insert 33, Insert 3]
 let new = runs actions old
 let inserted = unzipped new

 print old
 print new
 print inserted
