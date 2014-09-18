{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  QuasiQuotes #-}
module Main where

-- Import our template "pr"
import Template  ( debug )
import Language.Haskell.TH.Syntax

g x =        1+x
f x = [debug|1+x|] 

-- The splice operator $ takes the Haskell source code
-- generated at compile time by "pr" and splices it into
-- the argument of "putStrLn".
main = do 
 print $ g 2
 print $ f 2
