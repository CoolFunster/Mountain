module CopiedMountain.Data.AST where

import Prelude hiding (unwords)

import Data.Text (Text, unwords)
import qualified Data.Text as Text
import qualified Data.Map as M
import Data.List (intercalate)

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
  | ELabel Id Exp
  deriving (Eq, Ord, Show)

data Lit
  = LInt Integer
  | LBool Bool
  | LThing Text
  | LChar Char
  | LString String
  | LFloat Float
  deriving (Eq, Ord, Show)

data Type
  = TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TThing
  | TVar Text
  | TFun Type Type
  | TPair Type Type
  | TSum Type Type
  | TLabel Id Type
  deriving (Eq, Ord, Show)

data Pattern =
    PLit Lit
  | PVar Id
  | PPair Pattern Pattern
  | PLabel Id Pattern
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Text] Type


-- Ignore from here onwards
isFun :: Type -> Bool
isFun ty = case ty of
  TFun _ _ -> True
  _ -> False

renameVar :: Type -> (Text, Text) -> Type
renameVar ty (old, new) = case ty of
  TInt -> TInt
  TBool -> TBool
  TChar -> TChar
  TString -> TString
  TFloat -> TFloat
  TThing -> TThing
  TVar var -> TVar (if var == old then new else var)
  TFun t1 t2 -> TFun (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TPair t1 t2 -> TPair (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TSum t1 t2 -> TSum (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TLabel id t1 -> TLabel id (renameVar t1 (old, new))

expAsPattern :: Exp -> Pattern
expAsPattern (EVar id) = PVar id
expAsPattern (ELit l) = PLit l
expAsPattern (EPair a b) = PPair (expAsPattern a) (expAsPattern b)
expAsPattern (ELabel id exp) = PLabel id (expAsPattern exp)
expAsPattern t@(ELam _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(ELet _ _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EMatch _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EApp _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t

patternAsExp :: Pattern -> Exp
patternAsExp (PVar id) = EVar id
patternAsExp (PLit l) = ELit l
patternAsExp (PPair a b) = EPair (patternAsExp a) (patternAsExp b)
patternAsExp (PLabel id exp) = ELabel id (patternAsExp exp)