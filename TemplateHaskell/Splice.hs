{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Splice where
-- splice must be in a separate module to the module that calls it
import Language.Haskell.TH

-- http://

-- can't splice into types here
-- unit s = [d| data $s = $s deriving (Show) |]
--  where name = mkName s

-- DataD Cxt Name [TyVarBndr] [Con] [Name]
-- NormalC Name [StrictType]
unit s        = return [DataD context typename parameters constructors derived]
 where
 context      = []
 typename     = name
 parameters   = []
 constructors = [NormalC name [(NotStrict, ConT $ mkName "Int")]]
 derived      = [''Show]
 name         = mkName s

{-

-}
