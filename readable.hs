peek1 xs = [head xs]

peek2 = (: []) . head

peek3 :: [a] -> [a]
peek3 = return . head


main = do
 print $ peek1 "ABC"
 print $ peek2 "ABC"
 print $ peek3 "ABC"
