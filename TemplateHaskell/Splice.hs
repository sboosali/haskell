{-# LANGUAGE TemplateHaskell, QuasiQuotes, ExtendedDefaultRules #-}
module Splice where
-- splice must be in a separate module to the module that calls it

import Text.InterpolatedString.Perl6
import Language.Haskell.Meta.Parse

import Language.Haskell.TH
import Data.Either.Utils
import Control.Arrow

-- http://

-- DataD Cxt Name [TyVarBndr] [Con] [Name]
-- NormalC Name [StrictType]
unit :: String -> Q [Dec]
unit s        = return [DataD context typename parameters constructors derived]
 where
 context      = []
 typename     = name
 parameters   = []
 constructors = [NormalC name [(NotStrict, ConT $ mkName "Int")]]
 derived      = [''Show]
 name         = mkName s

-- can't splice into types here, must build string, then parse as declaration
-- string interpolation is easier to read than constructing ASTs
unit' :: String -> Q [Dec]
unit' = makeSingletonDeclaration >>> parseDecs >>> fromRight >>> return

makeSingletonDeclaration :: String -> String
makeSingletonDeclaration name = [qq|data {name} = {name} deriving (Show)|]
