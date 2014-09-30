import Control.Monad
-- http://book.realworldhaskell.org/read/error-handling.html


divs numerator = map (numerator `div`)

sdivs numerator = sequence . map (numerator `sdiv`)

sdiv numerator denominator
 | denominator == 0  = Left "division by zero"
 | otherwise         = Right $ numerator / denominator

{- $ ghci 

:set -XScopedTypeVariables
import Control.Exception
try :: IO a -> IO (Either Exception a)
try (evaluate undefined) :: IO (Either IOException a)
try (print undefined) :: IO (Either IOException ())
try (return undefined) :: IO (Either IOException a)
try (return 0) :: IO (Either IOException Int)

handle :: Exception e => (e -> IO a) -> IO a -> IO a
(\(_ :: IOException) -> putStrLn "[Error]") `handle` (print undefined)
(\(_ :: IOException) -> putStrLn "[Error]") `handle` (print 0)

throw :: Exception -> a
can throw any Exception in any (even pure) context

-}


main = do
 print $ 2 `sdivs` []
 print $ 2 `sdivs` [1,2,3]
 print $ 2 `sdivs` [1,0,3]
 print $ 2 `sdivs` [1,0,undefined]
