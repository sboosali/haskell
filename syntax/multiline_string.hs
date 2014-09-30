{-# LANGUAGE QuasiQuotes #-}
import Str -- template haskell quasi-quarters must be imported

-- $ runhaskell multiline_string


multiline = [str|This is a multiline string.


It contains embedded newlines. And unicode:
 
łe¶→łeđø→ħe¶ŋø→nđe”øn
 
It ends here |]


main = do
 putStrLn multiline
