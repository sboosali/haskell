{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where
import Template
import Splice

import Language.Haskell.TH.Syntax

-- $ cabal install haskell-src-exts
-- $ cabal install haskell-src-meta
-- $ cabal install MissingH

-- $ cabal exec runhaskell Main.hs 

g x =        1+x
f x = [debug|1+x|] 

unit "T"
unit' "S"

main = do
 print $ g 2
 print $ f 2
 print $ T 0
 print $ (makeSingletonDeclaration "S")
 print $ S
