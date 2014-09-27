{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where
import Template  ( debug )
import Language.Haskell.TH.Syntax


g x =        1+x
f x = [debug|1+x|] 


main = do 
 print $ g 2
 print $ f 2
