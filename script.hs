#!/usr/bin/env runhaskell
-- haskell as scripting language
-- $ chmod +
import System.Environment
import Data.Char

main = do
 [inputFile, outputFile] <- getArgs
 input <- readFile inputFile -- laziness -> can read huge files
 let output = map toUpper input -- supports unicode
 writeFile outputFile output
