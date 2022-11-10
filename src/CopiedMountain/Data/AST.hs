module CopiedMountain.Data.AST where

import CopiedMountain.Hash


import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (intercalate)


-- Main Lambda Calculus
type Id = String
type Env = M.Map Id Exp

data Exp =
  -- calculus
    EVar Id
  | EApp Exp Exp
  | ELam Pattern Exp
  | ELet Pattern Exp Exp
  | EAnnot Type Exp
  -- type statements
  | ETDef Id Type Exp
  -- data types
  | ELit Lit
  | ERec Id Exp
  | EMatch Exp Exp
  | EPair Exp Exp
  | ELabel Id Exp
  -- | EUnique Hash Exp
  deriving (Eq, Ord, Show)

data Lit
  =
    LUnit 
  | LInt Integer
  | LBool Bool
  | LThing String
  | LChar Char
  | LString String
  | LFloat Float
  deriving (Eq, Ord, Show)

data Type
  = 
  -- Builtins
    TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TThing
  | TUnit
  -- Calculus
  | TVar Id
  | TFun Type Type
  -- Data Structures
  | TPair Type Type
  | TSum Type Type
  | TLabel Id Type
  -- | TUnique Hash Type
  -- Kinds
  | TType Kind
  | TCall Type Type
  deriving (Eq, Ord, Show)

data Kind =
    KType
  | KFun Kind Kind
  | KApp Kind Kind
  deriving (Eq, Ord, Show)

data Pattern =
    PLit Lit
  | PVar Id
  | PPair Pattern Pattern
  | PLabel Id Pattern
  | PAnnot Type Pattern
  -- | PUnique Pattern
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Id] Type deriving (Show)

-- Ignore from here onwards
isFun :: Type -> Bool
isFun ty = case ty of
  TFun _ _ -> True
  _ -> False

renameVar :: Type -> (Id, Id) -> Type
renameVar ty (old, new) = case ty of
  TUnit -> TUnit
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
  TType x -> TType x
  TCall a b -> TCall a b
  -- TUnique h a -> TUnique h (renameVar a (old, new))

expAsPattern :: Exp -> Pattern
expAsPattern (EVar id) = PVar id
expAsPattern (ELit l) = PLit l
expAsPattern (EPair a b) = PPair (expAsPattern a) (expAsPattern b)
expAsPattern (ELabel id exp) = PLabel id (expAsPattern exp)
expAsPattern (EAnnot typ exp) = PAnnot typ (expAsPattern exp)
-- expAsPattern (EUnique h a) = PUnique (expAsPattern a)
expAsPattern t@(ETDef _ _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(ELam _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(ELet _ _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EMatch _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EApp _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(ERec _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t

patternAsExp :: Pattern -> Exp
patternAsExp (PVar id) = EVar id
patternAsExp (PLit l) = ELit l
patternAsExp (PPair a b) = EPair (patternAsExp a) (patternAsExp b)
patternAsExp (PLabel id exp) = ELabel id (patternAsExp exp)
patternAsExp (PAnnot typ x) = EAnnot typ (patternAsExp x)
-- patternAsExp (PUnique pat) = EUnique Unset (patternAsExp pat)