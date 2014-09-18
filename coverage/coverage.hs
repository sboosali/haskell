{- $

# bash
rm *.tix *.mix  &&  ghc -fhpc  coverage.hs --make  &&  ./coverage
hpc markup coverage  &&  open hpc_index.html  &&  open Main.hs.html

# fish
rm *.tix *.mix  ;and  ghc -fhpc  coverage.hs --make  ;and  ./coverage
hpc markup coverage  ;and  open hpc_index.html  ;and  open Main.hs.html

# yellow expressions were never evaluated
# white expressions were evaluated

# green boolean expressions were always true
# red boolean expressions were always false
# white boolean expressions were both true and false

-}


-- 100% line coverage and 100% branch coverage (but <100% condition coverage)
-- hpc measures line coverage and branch coverage (but not condition coverage) 
-- where "branch" means "any guard, condition, qualifier"
-- where "condition" means "every boolean subexpression"
-- http://en.wikipedia.org/wiki/Code_coverage#Coverage_criteria
-- it looks all right (all white)
-- but an uncovered condition has a buggy expression
f x y
 | x==0 && y==0 || x>0  && y>0   =  x / y
 | x==0 && y>0  || x>0  && y==0  =  x + y
 | otherwise                     =  x - y

-- this function is logically equivalent
-- it reports <100% branch coverage
-- we see the uncovered branch in red
-- we see the unevaluated subexpression in yellow 
f' x y
 | x==0 && y==0  =  x / y
 | x>0  && y>0   =  x / y
 | x==0 && y>0   =  x + y
 | x>0  && y==0  =  x + y
 | otherwise     =  x - y


main = do
-- print $ f 0 0
 print $ f 0 1
 print $ f 1 0
 print $ f 1 1
 print $ f (-1) (-1)

-- print $ f' 0 0
 print $ f' 0 1
 print $ f' 1 0
 print $ f' 1 1
 print $ f'  (-1) (-1)
