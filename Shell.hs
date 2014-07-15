#!/usr/bin/env runhaskell
import HSH

main = do
 runIO $ "ls -l" -|- "wc -l"
