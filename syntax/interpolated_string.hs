{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
import Text.InterpolatedString.Perl6
-- $ cabal install happy alex # some issue with installing executables
-- $ cabal install interpolatedstring-perl6
-- $ cabal exec runhaskell interpolated_string 


x = 1
y = 3
z = [qq|
x + 2 = {x + 2}
y = $y
|]


main = do
 putStrLn z
