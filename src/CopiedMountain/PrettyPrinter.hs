{-# language OverloadedStrings #-}
module CopiedMountain.PrettyPrinter where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Log
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Char (toLower)
import Data.List (intercalate)

prettyLit :: Lit -> String
prettyLit (LInt i) = show i
prettyLit (LBool b) = map toLower $ show b

prettyPattern :: Pattern -> String
prettyPattern (PLit i) = prettyLit i
prettyPattern (PVar v) = T.unpack v
prettyPattern (PPair a b) = "(" ++ prettyPattern a ++ "," ++ prettyPattern b ++ ")"
prettyPattern (PRecord omap) = "{" <> intercalate "," (map (\(a,b) -> T.unpack a <> ":" <> prettyPattern b) (M.toList omap)) <> "}"

prettyExp :: Exp -> String
prettyExp (ELit l) = prettyLit l
prettyExp (EVar id) = T.unpack id 
prettyExp (EApp a b) = prettyExp a ++ "(" ++ prettyExp b ++ ")"
prettyExp (ELam id e) = prettyPattern id ++ "->" ++ prettyExp e
prettyExp (ELet id a b) = T.unpack id ++ "=" ++ prettyExp a ++ ";" ++ prettyExp b
prettyExp (EMatch a b) = "(" ++ prettyExp a ++ "||" ++ prettyExp b ++ ")"
prettyExp (EPair a b) = "(" ++ prettyExp a ++ "||" ++ prettyExp b ++ ")"
prettyExp (ERecord omap) = "{" <> intercalate "," (map (\(a,b) -> T.unpack a <> ":" <> prettyExp b) (M.toList omap)) <> "}"

prettyType :: Type -> T.Text
prettyType ty = case ty of
  TVar var -> var
  TInt -> "Int"
  TBool -> "Bool"
  TFun ty1 ty2 ->
    (if isFun ty1 then "(" <> prettyType ty1 <> ")" else prettyType ty1)
    <> " -> " <> prettyType ty2
  TPair ty1 ty2 -> "(" <> prettyType ty1 <> "," <> prettyType ty2 <> ")"
  TRecord omap -> "{" <> T.intercalate "," (map (\(a,b) -> a <> ":" <> prettyType b) (M.toList omap)) <> "}"

prettyScheme :: Scheme -> T.Text
prettyScheme (Scheme [] ty) = prettyType ty
prettyScheme (Scheme vars ty) =
  let
    -- This means we can only print types with a maximum of 26 type
    -- variables (Should be enough for the talk :D)
    vars' = zip vars (map T.singleton ['a'..'z'])
    renamedTy = foldl renameVar ty vars'
  in
    "forall " <> T.unwords (map snd vars') <> ". " <> prettyType renamedTy

prettyLog :: [Log] -> String
prettyLog (Step expr env:xs) = do
  prettyExp expr ++ "\n" ++ prettyLog xs
prettyLog [] = "END"