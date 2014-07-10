-- must build Id from String (for your functions that take Id)
newtype Id = Id String
unId (Id s) = s

-- can use any String as Identifier (for your functions that take Identifier)
-- can use any Identifier as String (for builtins that take String)
type Identifier = String
identifier :: String -> Identifier
identifier s = s

main = do
 print $ "x" ++ " : String"
 --ERR print $ Id "(Id 'y')" ++ " : Id"
 print $ unId (Id "(Id 'y')") ++ " : Id (not String)"
 print $ identifier "identifier s" ++ " : Identifier (and String)"

