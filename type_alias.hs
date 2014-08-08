-- must build Id from String (for your functions that take Id)
newtype Id = Id String
unId (Id s) = s

-- can use any String as Identifier (for your functions that take Identifier)
-- can use any Identifier as String (for builtins that take String)
type Identifier = String
identifier :: String -> Identifier
identifier s = s

-- `newtype` boxes a type, you must manually unbox it
-- `type` aliases a type, the compiler compiles it away

m
main = do
 print $ "x" ++ " : String"
 print $ unId (Id "(Id 'y')") ++ " : Id (not String)"
 print $ identifier "identifier s" ++ " : Identifier (and String)"

 {--
 print $ Id "(Id 'y')" ++ " : Id"
 {--}
