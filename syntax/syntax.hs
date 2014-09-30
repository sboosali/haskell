{-# LANGUAGE PostfixOperators, TupleSections, UnicodeSyntax, LambdaCase  #-}
import Data.List.Unicode ((∪))
import Control.Monad
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions
-- $ runhaskell syntax


(!) = product . enumFromTo 1


main = do
 print $ (4 !)
 print $ ((!) 4)

 print $ (,2,) 1 3
 print $ (\x y -> (x,2,y)) 1 3

 print $ [1, 2, 3] ∪ [1, 3, 5]

 [Nothing, Just 1] `forM_` \case          -- "\x -> case x of"
    Just v  -> print ("just " ++ show v)
    Nothing -> print "nothing"
