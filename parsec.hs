import Text.ParserCombinators.Parsec
import System.Environment


-- Parsec: basic CSV parsing, in 4 lines of Haskell

csv  = endBy line (char '\n')
line = sepBy cell (char ',')
cell = many (noneOf ",\n")

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csv "error" input

-- sepBy x ','  matches  x,x
-- endBy x ','  matches  x,x,


main = do
 [file]   <- getArgs
 text     <- readFile file
 let tree = parseCSV text

 print text
 print tree
