{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Typeable
import qualified Data.Map as M
import Data.Map (Map)

-- http://chrisdone.com/posts/data-typeable
-- runhaskell dynamic.hs


data Person = Person Integer String
  deriving (Typeable, Show)
{-

class Typeable a where
 typeOf :: a -> TypeRep

cast :: (Typeable a, Typeable b) => a -> Maybe b

-}


-- existentially typed
-- a universal type
data Value = forall v. Typeable v => Value v

-- a Value can be any type:
--  the only thing we know about what's in the Value, is that it's Typeable
--  the only thing we can do with the Typeable, is to cast it
glookup :: (Typeable a, Ord key) => key -> Map key Value -> Maybe a
glookup key map = case M.lookup key map of
  Nothing        -> Nothing
  Just (Value x) -> (cast x)

main = do
  print (glookup "foo" map :: Maybe ())
  print (glookup "bar" map :: Maybe Char)
  print (glookup "chris" map :: Maybe Person)

    where map = M.insert "bar" (Value 'a')
              $ M.insert "foo" (Value ())
              $ M.insert "chris" (Value (Person 123 "Chris Done"))
              $ M.empty
