fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = print $ take 10 fibs
