import Data.Ord
import Data.Monoid
import Data.List
-- http://www.quora.com/What-does-one-mean-by-elegant-code/answer/Tikhon-Jelvis
-- $ runhaskell monoid


data Person = Person {
 firstName  :: String,
 middleName :: Maybe String,
 lastName   :: String
} deriving (Show)

compareNames = comparing lastName <> comparing firstName <> comparing middleName


main = do
 let he = Person "Leonard" Nothing "Cohen"
 let me = Person "Spiros" (Just "Michael") "Boosalis"
 let it = Person "Spiros" Nothing "Boosalis"

 print $ sortBy compareNames [he, me, it]
