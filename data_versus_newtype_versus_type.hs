{-# LANGUAGE OverloadedStrings #-}
module Main where

-- http://stackoverflow.com/a/5889784/1337806
-- http://stackoverflow.com/questions/2649305/why-is-there-data-and-newtype-in-haskell/2650051#2650051
-- $ runhaskell data_versus_newtype_versus_type.hs


data Data a = Data Int
newtype Newtype a = Newtype Int
type Type = Int

dataMatcher :: Data -> String
dataMatcher (Data _) = "data"

newtypeMatcher :: Newtype -> String 
newtypeMatcher (Newtype _) = "newtype"

typeNonMatcher _ = "type"


main = do
 print $ newtypeMatcher undefined -- "newtype"
 print $ dataMatcher undefined    -- ERROR: undefined 
