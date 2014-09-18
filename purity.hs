{-# LANGUAGE OverloadedStrings #-}
import Prelude
import Control.Applicative

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.Map as Map
import Data.Map (Map)
-- http://www.haskell.org/haskellwiki/Avoiding_IO


-- a monadic function
-- you can only call this function within the IO monad
translate' :: Text -> IO Text
translate' word = do
 dict <- readDictionary "data/dictionary.txt"
 return (Map.findWithDefault word word dict)


-- a monadic value
-- @return@s a pure function
mkTranslator :: IO (Text -> Text)
mkTranslator = do
 dict <- readDictionary "data/dictionary.txt"
 return $ translate dict

-- pure functions
translate dict word = Map.findWithDefault word word dict
translates translator = T.unwords . map translator . T.words


readDictionary :: String -> IO (Map Text Text)
readDictionary path = do
 text <- readFile path
 return $ Map.fromList [(english, french) | (english:french:_) <- map parse $ T.lines $ T.pack text]
 where parse = map T.strip . T.splitOn "->"


main :: IO ()
main = do
 translator <- mkTranslator
 print $ translates translator "translate this sentence"

-- "I call this Applicative Functor style because you can use the application operator from @Control.Applicative@: @makeTranslator <*> getLine@"
