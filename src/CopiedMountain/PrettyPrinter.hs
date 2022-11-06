module CopiedMountain.PrettyPrinter where

import CopiedMountain.Data.AST
import qualified Data.Text as T
import Data.Char (toLower)

prettyLit :: Lit -> String
prettyLit (LInt i) = show i
prettyLit (LBool b) = map toLower $ show b

prettyPattern :: Pattern -> String
prettyPattern (PLit i) = prettyLit i
prettyPattern (PVar v) = T.unpack v

prettyExp :: Exp -> String
prettyExp (ELit l) = prettyLit l
prettyExp (EVar id) = T.unpack id 
prettyExp (EApp a b) = prettyExp a ++ "(" ++ prettyExp b ++ ")"
prettyExp (ELam id e) = prettyPattern id ++ "->" ++ prettyExp e
prettyExp (ELet id a b) = T.unpack id ++ "=" ++ prettyExp a ++ ";" ++ prettyExp b