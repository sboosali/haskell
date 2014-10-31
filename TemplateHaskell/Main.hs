{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where
import Template ( debug )
import Splice ( unit )

import Language.Haskell.TH.Syntax

-- $ cabal exec runhaskell Main.hs 


g x =        1+x
f x = [debug|1+x|] 

unit "T"


main = do 
 print $ g 2
 print $ f 2
 print $ T 0
