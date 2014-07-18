import Language.Haskell.Exts hiding (parse)
import System.Environment

parse :: String -> Module
parse = fromParseResult . parseFileContents

main = do
 [file] <- getArgs
 text <- readFile file
 let ast = parse text
 print ast

{- h parsing_haskell.hs parsing_haskell.hs

Module
 (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 1, srcColumn = 1})
 (ModuleName "Main")
 []
 Nothing
 (Just [EVar (UnQual (Ident "main"))])

 [ImportDecl {importLoc = SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 1, srcColumn = 1}, importModule = ModuleName "Language.Haskell.Exts", importQualified = False, importSrc = False, importPkg = Nothing, importAs = Nothing, importSpecs = Just (True,[IVar (Ident "parse")])},

  ImportDecl {importLoc = SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 2, srcColumn = 1}, importModule = ModuleName "System.Environment", importQualified = False, importSrc = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}]

 [TypeSig (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 4, srcColumn = 1})
          [Ident "parse"]
          (TyFun (TyCon (UnQual (Ident "String"))) (TyCon (UnQual (Ident "Module")))),

  PatBind (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 5, srcColumn = 1})
          (PVar (Ident "parse"))
          Nothing
          (UnGuardedRhs (InfixApp (Var (UnQual (Ident "fromParseResult")))
                                  (QVarOp (UnQual (Symbol ".")))
                                  (Var (UnQual (Ident "parseFileContents")))))
          (BDecls []),

  PatBind (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 7, srcColumn = 1})
          (PVar (Ident "main"))
          Nothing
          (UnGuardedRhs (Do
            [Generator (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 8, srcColumn = 2})
                       (PList [PVar (Ident "file")])
                       (Var (UnQual (Ident "getArgs"))),

             Generator (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 9, srcColumn = 2})
                       (PVar (Ident "text"))
                       (App (Var (UnQual (Ident "readFile"))) (Var (UnQual (Ident "file")))),

             LetStmt (BDecls [PatBind (SrcLoc {srcFilename = "parsing_haskell.hs", srcLine = 10, srcColumn = 6})
                                      (PVar (Ident "ast"))
                                      Nothing
                                      (UnGuardedRhs (App (Var (UnQual (Ident "parse"))) (Var (UnQual (Ident "text")))))
                                      (BDecls [])]),

             Qualifier (App (Var (UnQual (Ident "print"))) (Var (UnQual (Ident "ast"))))]))

          (BDecls [])]

-}
