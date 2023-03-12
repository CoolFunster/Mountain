{-# language OverloadedStrings #-}
module CopiedMountain.PrettyPrinter where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Errors
import CopiedMountain.Data.Log
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

import Data.Char (toLower)
import Data.List (intercalate)

prettyLit :: Lit -> String
prettyLit LUnit = "()"
prettyLit (LInt i) = show i
prettyLit (LBool b) = map toLower $ show b
prettyLit (LThing t) = "#" ++ t
prettyLit (LChar c) = "'" ++ [c] ++ "'"
prettyLit (LString s) = "\"" ++ s ++ "\""
prettyLit (LFloat f) = show f

prettyPattern :: Pattern -> String
prettyPattern (PLit i) = prettyLit i
prettyPattern (PVar v) = v
prettyPattern (PPair a b) = "(" ++ prettyPattern a ++ "," ++ prettyPattern b ++ ")"
prettyPattern (PLabel id x) = id ++ ":" ++ prettyPattern x
prettyPattern (PAnnot typ x) = "(" ++ prettyType typ ++ "::" ++ prettyPattern x ++ ")"
prettyPattern PWildcard = "_"

prettyExp :: Exp -> String
prettyExp (ELit l) = prettyLit l
prettyExp (EVar id) = id
prettyExp (EApp a b) = "(" ++ prettyExp a ++ ")(" ++ prettyExp b ++ ")"
prettyExp (EFun id e) = prettyPattern id ++ "->" ++ prettyExp e
prettyExp (ELet pat a b) = prettyPattern pat ++ "=" ++ prettyExp a ++ ";" ++ prettyExp b
prettyExp (EMatch a b) = "(" ++ prettyExp a ++ "||" ++ prettyExp b ++ ")"
prettyExp (EPair a b) = "(" ++ prettyExp a ++ "," ++ prettyExp b ++ ")"
prettyExp (ELabel id a) = id ++ ":" ++ prettyExp a
prettyExp (EAnnot typ a) = "(" ++ prettyType typ ++ ")::(" ++ prettyExp a ++ ")"
prettyExp (ERec id a) = "(" ++ id ++ "~" ++ prettyExp a ++ ")"
prettyExp (ETDef id typ rest) = "type " ++ id ++ " = " ++ prettyType typ ++ ";" ++ prettyExp rest
prettyExp (EToken id _) = "#" <> id
prettyExp (EModule stmts) = "<{" <> concatMap ((++) ";" . prettyModuleStmt) stmts <> "}>"

prettyModuleStmt :: ModuleStmt -> [Char]
prettyModuleStmt stmt = case stmt of 
  MTypeDec s ki -> "tdec " <> s <> " = " <> prettyKind ki 
  MTypeDef s ty -> "type " <> s <> " = " <> prettyType ty
  MValDec s ty -> "dec " <> s <> " = " <> prettyType ty
  MValDef s exp -> "val " <> s <> " = " <> prettyExp exp
  MImport s -> "import " <> s
  MLoad exp -> "import " <> prettyExp exp

prettyKind :: Kind -> String
prettyKind KType = "Type"
prettyKind (KFun a b) = prettyKind a  ++ "->" ++ prettyKind b
prettyKind (KApp a b) = "(" ++ prettyKind a ++ ")(" ++ prettyKind b ++ ")"

prettyUseCount :: UseCount -> String
prettyUseCount CSingle = "?"
prettyUseCount CMany = "+"
prettyUseCount CAny = ""

prettyType :: Type -> String
prettyType ty = case ty of
  TVar var -> var
  TInt -> "Int"
  TBool -> "Bool"
  TChar -> "Char"
  TString -> "String"
  TFloat -> "Float"
  TThing -> "Thing"
  TUnit -> "()"
  TFun ty1 ty2 ->
    (if isTFun ty1 then "(" <> prettyType ty1 <> ")" else prettyType ty1)
    <> " -> " <> prettyType ty2
  TPair ty1 ty2 -> "(" <> prettyType ty1 <> "," <> prettyType ty2 <> ")"
  TSum ty1 ty2 -> "(" <> prettyType ty1 <> "|" <> prettyType ty2 <> ")"
  TLabel id x -> id <> ":" <> prettyType x
  TType kind -> prettyKind kind
  TCall a b -> "(" <> prettyType a <> ")(" <> prettyType b <> ")"
  TToken id -> "$" <> id
  TUsage u t -> prettyUseCount u <> prettyType t
  TInterface stmts -> "<{" <> concatMap ((++) ";" . prettyModuleStmt) stmts <> "}>"

prettyScheme :: Scheme -> String
prettyScheme (Scheme [] ty) = prettyType ty
prettyScheme (Scheme vars ty) =
  let
    -- This means we can only print types with a maximum of 26 type
    -- variables (Should be enough for the talk :D)
    vars' = zip vars (map (T.unpack . T.singleton) ['a'..'z'])
    renamedTy = foldl renameVar ty vars'
  in
    "forall " <> unwords (map snd vars') <> ". " <> prettyType renamedTy

prettyLog :: [Log] -> String
prettyLog (Step expr env:xs) = do
  prettyExp expr ++ "\n" ++ prettyLog xs
prettyLog [] = "END"

prettyError :: Error -> String
prettyError = show