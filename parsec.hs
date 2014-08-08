import Text.ParserCombinators.Parsec
import System.Environment
import Control.Arrow
-- http://book.realworldhaskell.org/read/using-parsec.html


-- Parsec: basic CSV parsing, in 4 lines of Haskell

-- sepBy x ','  matches  x,x
-- endBy x ','  matches  x,x,

_csv  = endBy _line (char '\n')
_line = sepBy _cell (char ',')
_cell = many (noneOf ",\n")

_parseCSV :: String -> Either ParseError [[String]]
_parseCSV input = parse _csv "error" input


-- Parsec: complete/configurable CSV parsing, in _ lines of Haskell

-- `sepEndBy x '-'` matches both `x-x` and `x-x-`

csv  = sepEndBy line eol
line = sepBy cell sep
cell = quotedCell <|> unquotedCell
-- `sepEndBy` for optional trailing newline
--TODO schema + fixed length

end_of_lines = ["\n", "\r", "\n\r"]
anyOf xs = foldl1 (<|>) $ map (try . string) xs
eol = anyOf end_of_lines <?> "end of line"
-- configurable line-endings
-- `try (string _) <|> try (string _) <?> _`
-- try each string, maybe fail

seperator = ','
sep = char seperator
-- configurable seperator

quote = '"'
escaped_quote = [quote, quote]

quotedCell = do
 char quote
 text <- many quotedChar
 char quote
 return text

quotedChar = try (string escaped_quote >> return quote)  <|>  noneOf [quote]

delimiters = seperator : foldl1 (++) end_of_lines
unquotedCell = many $ noneOf delimiters

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = right (filter (/= [""])) $ parse csv "error" input
-- ignore blanks


-- Parsec is a "combinator" library
--
-- BNF
-- float ::= sign? digit+ ('.' digit+)?
--
-- Haskell
-- float :: Parser
-- float = optional sign
--     <*> oneOrMore digit
--     <*> optional (lit ’.’ <*> oneOrMore digit)
--
-- Haskell
-- float :: Parser Float
-- float = do
--  _sign   <- optional sign
--  _digits <- oneOrMore digit
--  _fracs  <- optional (do lit '.'
--                          oneOrMore digit)
--  return (_float _sign _digits _fracs)
--
-- "the combinators `optional`, `oneOrMore`, `(<*>)` combine parsers to make bigger parsers"

main = do
 [file]   <- getArgs
 text     <- readFile file
 let tree = parseCSV text

 let div = putStrLn "--------------------------------------"
 let blank = putStrLn ""

 blank
 div
 putStr text
 div
 blank

 print tree
