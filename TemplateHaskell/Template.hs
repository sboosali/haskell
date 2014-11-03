{-# LANGUAGE TemplateHaskell #-}
module Template where
-- quasi-quoter must be in a separate module to the module that uses it
import Language.Haskell.TH -- import some Template Haskell syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Debug.Trace
-- http://www.haskell.org/ghc/docs/latest/html/users_guide/template-haskell.html#th-example


{-

Data QuasiQuoter = QuasiQuoter {	 
 quoteExp  :: String -> Q Exp
 quotePat  :: String -> Q Pat
 quoteType :: String -> Q Type
 quoteDec  :: String -> Q [Dec]
}

newtype Q a = Q { unQ :: forall m. Quasi m => m a }

data Exp =
 VarE Name                          -- { x }
 ConE Name                          -- data T1 = C1 t1 t2; p = {C1} e1 e2
 LitE Lit                           -- { 5 or c}
 AppE Exp Exp                       -- { f x }
 InfixE (Maybe Exp) Exp (Maybe Exp) -- {x + y} or {(x+)} or {(+ x)} or {(+)}
 UInfixE Exp Exp Exp                -- {x + y}
 ParensE Exp                        -- { (e) }
 LamE [Pat] Exp                     -- {  p1 p2 -> e }
 TupE [Exp]                         -- { (e1,e2) }
 UnboxedTupE [Exp]                  -- { () }
 CondE Exp Exp Exp                  -- { if e1 then e2 else e3 }
 LetE [Dec] Exp                     -- { let x=e1;   y=e2 in e3 }
 CaseE Exp [Match]                  -- { case e of m1; m2 }
 DoE [Stmt]                         -- { do { p <- e1; e2 }  }
 CompE [Stmt]                       -- { [ (x,y) | x <- xs, y <- ys ] }
 ArithSeqE Range                    -- { [ 1 ,2 .. 10 ] }
 ListE [Exp]                        -- { [1,2,3] }
 SigE Exp Type                      -- { e :: t }
 RecConE Name [FieldExp]            -- { T { x = y, z = w } }
 RecUpdE Exp [FieldExp]             -- { (f x) { z = w } }

-}



-- makes Haskell source code
-- will be spliced into the module that uses it at compile time
debug :: QuasiQuoter
debug = QuasiQuoter traceExp undefined undefined undefined
-- [debug| 1+2 |] == $(quoteExp debug " 1+2 ")
-- defined for expression syntax contexts
-- undefined for pattern/type/declaration syntax contexts
-- @undefined@ is Haskell's null


-- e.g. trace "1+2 = {1+2}" (1+2)
traceExp :: String -> Q Exp
traceExp s = [|( trace $(showExp s) $(parseE s) )|]
-- [| ... |] is like lisp quasi-quoting
-- $( ... ) is like lisp un-quoting i.e. splicing
-- both @String@s and @Q Exp@s are like lisp @sexp@
-- in clojure, it would be `(trace (str (quote ~s) " = " ~s) ~s) with safety and stuff

-- "1+2" ++ " = " ++ show (1+2)
showExp :: String -> Q Exp
showExp s = [|( $(stringE s) ++ " = " ++ show $(parseE s) )|]
-- shows symmetry between stringE and parseE

{-

stringE :: String -> Q Exp

parseExp :: String -> Either String Exp

-}

parseE :: String -> Q Exp
parseE = return . right . parseExp
 where right = either (const undefined) id  -- partial function oh no
