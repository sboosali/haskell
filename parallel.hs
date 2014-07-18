module Parallel where
import Control.Parallel.Strategies hiding (parMap)

-- $ ghc  parallel.hs  -o parallel.hout  -main-is Parallel

-- $ time ./sequential.hout
-- 11s

-- $ time ./parallel.hout
-- 9s


parMap f xs = map f xs `using` parList rseq


-- slow function
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

slow = [fib 35, fib 35]


main = do
 print $ parMap id slow
