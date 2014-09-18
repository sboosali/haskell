-- http://book.realworldhaskell.org/read/profiling-and-optimization.html
-- https://www.haskell.org/ghc/docs/7.6.3/html/users_guide/profiling.html
-- http://stackoverflow.com/questions/3276240/tools-for-analyzing-performance-of-a-haskell-program

-- $ ghc  -prof -fprof-auto -caf-all -rtsopts  profile.hs
-- $ ./profile +RTS -p -hc
-- $ cat profile.prof
-- $ cat profile.hp

-- ghc
-- -prof         enable profiling
--               (all modules to be profiled, must be compiled with this option)
-- -fprof-auto   all bindings are cost-centers
--               (not marked INLINE, whether exported or not, top level or nested)
-- -caf-all      all constant applicative functions are cost-centers
-- -rtsopts      enable run-time system options

-- -p            profile time
-- -hc           profile space
--               time-uniform sampling
-- -iN           sampling rate (seconds)

-- {-# SCC "name" #-} "SCC-annotation" makes the expression to the right a cost-center


fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)


main = print (f 30 + g 30)
 where
  f n = {-# SCC "large" #-} fib n
  g n = {-# SCC "small" #-} fib (n `div` 2)
