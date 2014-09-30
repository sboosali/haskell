{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
import Control.Monad
import Control.Arrow
import Data.List
import Control.Lens

-- https://github.com/gcross/LogicGrowsOnTrees/blob/master/README.md
-- http://stackoverflow.com/questions/15530511/how-to-edit-nth-element-in-a-haskell-list


{- Logic Program -}

-- the problem: the number of ways of putting n queens on an n-by-n board
-- http://en.wikipedia.org/wiki/Eight_queens_puzzle
nqueensCount = length . nqueens

type Positions = [(Int, Int)]

nqueens :: Int -> [Positions]
nqueens n
 | n > 0      = nqueens' n 1 [1..n] [] [] []
 | otherwise  = error "nqueens: board size must be positive"
 -- Start with...
 -- ... n queens left
 -- ... at row zero
 -- ... with all columns available
 -- ... with no occupied negative diagonals
 -- ... with no occupied positive diagonals

-- placed last queen
nqueens' 0 _ _ _ _ solutions = return solutions
-- still placing queens
nqueens'
 n
 row
 available_columns
 occupied_negative_diagonals
 occupied_positive_diagonals
 solutions
 = do

 -- pick one
 -- the list monad represents nondeterministic computation
 column <- available_columns

 -- check it
 let negative_diagonal = row + column
 guard $ not $ negative_diagonal `elem` occupied_negative_diagonals

 -- check it
 let positive_diagonal = row - column
 guard $ not $ positive_diagonal `elem` occupied_positive_diagonals

 -- place here
 -- solve subproblem
 nqueens'
  (n-1)
  (row+1)
  (filter (/= column) available_columns)
  (negative_diagonal : occupied_negative_diagonals)
  (positive_diagonal : occupied_positive_diagonals)
  ((row, column) : solutions)

 -- constraint: queens can share no row, column, positive diagonal, negative diagonal
 -- optimization: squares on horizontals/verticals are implicitly avoided
 -- optimization: every square on a diagonal reduces to the same sum/difference 


{- Visualization -}

data Matrix a = Matrix {fromMatrix :: [[a]]} 
instance Show a => Show (Matrix a) where
 show = fromMatrix >>> map (map show >>> intercalate " ") >>> intercalate "\n"

zero n = replicate n $ replicate n 0

placeQueens :: Positions -> Matrix Int
placeQueens spots = Matrix matrix
 where
 matrix = foldl place (zero n) spots
 -- foldl :: (b -> a -> b) -> b -> [a] -> b 
 place board (i, j) = set (element i . element j) 1 board
 -- PLACE (the queen) onto the BOARD at the SPOT
 -- set :: lens -> part -> whole -> _
 n = length spots

printQueens n = mapM_ (\matrix -> print matrix >> putStrLn "") $ map placeQueens (nqueens n)


{- Execution -}

main = do

 print $ nqueensCount 1  -- one queen one spot
 print $ nqueensCount 2  -- first queen takes whole board, second has no spot
 print $ nqueensCount 3
 print $ nqueensCount 4
 print $ nqueens 4
 print $ nqueensCount 8  -- chess

 putStrLn ""
 print $ zero 4

 putStrLn ""
 printQueens 8

