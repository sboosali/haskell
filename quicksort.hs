quicksort [] = []
quicksort (y:ys) = xs ++ [y] ++ zs
 where
 xs = quicksort (filter smaller ys)
 zs = quicksort (filter bigger  ys)
 smaller x = x <  y
 bigger  z = y <= z

main = print $ quicksort [4,3,6,2,5,1]
