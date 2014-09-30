{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ParallelListComp #-}
import GHC.Exts

-- $ runhaskell monad_comprehensions


monad_comprehension = [ x + y | x <- Just 1,
                                y <- Just 2 ]

guarded = [ x | x <- [1..10],
                x <= 5 ]

take_transfrom = [ x+y | x <- [1..10],
                         y <- [1..x],
                         then take 2 ]

group_transform = [ x | x <- [1,1,2,2,3],
                        then group by x using GHC.Exts.groupWith ]

-- i.e. zip
parallel = [ (x+y) | x <- [1..10]
                   | y <- [11..20]]

main = do
 print monad_comprehension
 print guarded
 print take_transfrom
 print group_transform
 print parallel
