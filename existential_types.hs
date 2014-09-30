{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Main where
import Control.Applicative
-- http://chrisdone.com/posts/rankntypes
-- $ runhaskell existential_types.hs


newtype DB c a = DB (IO a)
 deriving (Monad, Applicative, Functor)

newtype Connection c = Connection ()

withConnection :: (forall c. DB c a) -> IO a
withConnection m = case m of DB io -> io

getConn :: DB s (Connection c)
getConn = return (Connection ())

query :: Connection c -> String -> DB c [String]
query _ _ = return ["Existential Types!"]

demo :: IO [String]
demo = withConnection $ do
  conn <- getConn
  rows <- query conn "SELECT …"
  return rows

{-

-- you can't return the connection, because it was existentially typed
-- otherwise the block would have type "DB c c", which contradicts "forall a. (forall c. DB c a)"
escape = withConnection $ do
  conn <- getConn
  rows <- query conn "SELECT …"
  return conn

-- ERROR: inferred type is less polymorphic than expected
--        quantified type variable 'c' escapes

-}


main = do
 results <- demo
 print $ results
