-- Maybe statically avoids null pointer exceptions
data X = X { getY :: Maybe Y } deriving (Show)
data Y = Y { getZ :: Maybe Int } deriving (Show)

-- Maybe, a Monad, has sugar for chained possibly failing access
-- like Groovy's `x.?y.?z + 1`, but general for any Monad
get x = do
 y <- getY x
 z <- getZ y
 return $ z + 1


-- Groovy `.?`, hack
(.?) :: Maybe a -> (a -> Maybe b) -> Maybe b
(.?) = (>>=)
infixl 2 .?

into = flip fmap
infixl 2 `into`


main = do
 let x1 = X (Just $ Y (Just 0))
 let x2 = X (Just $ Y Nothing)
 let x3 = X Nothing
 let z4 = Just x1 .? getY .? getZ `into` (+2)

 print $ get x1
 print $ get x2
 print $ get x3
 print $ z4
