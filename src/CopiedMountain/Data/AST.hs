{-# language OverloadedStrings #-}
module CopiedMountain.Data.AST where

import Prelude hiding (unwords)

import Data.Text (Text, unwords)
import qualified Data.Text as Text
import qualified Data.Map as M

type Id = Text
type Env = M.Map Id Exp

data Exp = 
    ELit Lit
  | EVar Id
  | EApp Exp Exp
  | ELam Pattern Exp
  | ELet Id Exp Exp
  | EMatch Exp Exp
  | EPair Exp Exp
  deriving (Eq, Ord, Show)

data Lit
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord, Show)

data Type
  = TInt
  | TBool
  | TVar Text
  | TFun Type Type
  | TPair Type Type
  deriving (Eq, Ord, Show)

data Pattern =
    PLit Lit
  | PVar Id
  | PPair Pattern Pattern
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Text] Type


-- Ignore from here onwards
isFun :: Type -> Bool
isFun ty = case ty of
  TFun _ _ -> True
  _ -> False

prettyType :: Type -> Text
prettyType ty = case ty of
  TVar var -> var
  TInt -> "Int"
  TBool -> "Bool"
  TFun ty1 ty2 ->
    (if isFun ty1 then "(" <> prettyType ty1 <> ")" else prettyType ty1)
    <> " -> " <> prettyType ty2
  TPair ty1 ty2 -> "(" <> prettyType ty1 <> "," <> prettyType ty2 <> ")"

prettyScheme :: Scheme -> Text
prettyScheme (Scheme [] ty) = prettyType ty
prettyScheme (Scheme vars ty) =
  let
    -- This means we can only print types with a maximum of 26 type
    -- variables (Should be enough for the talk :D)
    vars' = zip vars (map Text.singleton ['a'..'z'])
    renamedTy = foldl renameVar ty vars'
  in
    "forall " <> unwords (map snd vars') <> ". " <> prettyType renamedTy

renameVar :: Type -> (Text, Text) -> Type
renameVar ty (old, new) = case ty of
  TInt -> TInt
  TBool -> TBool
  TVar var -> TVar (if var == old then new else var)
  TFun t1 t2 -> TFun (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TPair t1 t2 -> TPair (renameVar t1 (old, new)) (renameVar t2 (old, new))

expAsPattern :: Exp -> Pattern
expAsPattern (EVar id) = PVar id
expAsPattern (ELit l) = PLit l
expAsPattern (EPair a b) = PPair (expAsPattern a) (expAsPattern b)
expAsPattern t@(ELam _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(ELet _ _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EMatch _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EApp _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t

patternAsExp :: Pattern -> Exp
patternAsExp (PVar id) = EVar id
patternAsExp (PLit l) = ELit l
patternAsExp (PPair a b) = EPair (patternAsExp a) (patternAsExp b)