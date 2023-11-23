{-# LANGUAGE ScopedTypeVariables #-}
module Transpilers.CppTranspiler where

import AST

import Language.C
import Language.C.Data.Node
import Language.C.Data.Ident
import Language.C.Pretty
import Language.C.Syntax.AST
import Data.List

-- Create a simple function definition
createFunctionDefinition :: CTranslationUnit NodeInfo
createFunctionDefinition =
    CTranslUnit
        [ CFDefExt
            (CFunDef
                [ CTypeSpec (CIntType undefNode) ]
                (CDeclr (Just (internalIdent "x")) [] Nothing [] undefNode)
                []
                (CCompound
                    []
                    [ CBlockStmt $ CReturn
                        (Just (CConst (CIntConst (cInteger 8) undefNode)))
                        undefNode
                    ]
                    undefNode
                )
                undefNode
            )
        ]
        undefNode


main :: IO ()
main = print $ pretty createFunctionDefinition