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
  | ELam Pattern Exp
  | EULam Pattern Exp
  | EApp Exp Exp
  | ELet Pattern Exp Exp
  | EULet Pattern Exp Exp
  | EAnnot Type Exp
  -- type statements
  | ETDef Id Type Exp
  -- data types
  | ELit Lit
  | ERec Id Exp
  | EMatch Exp Exp
  | EPair Exp Exp
  | ELabel Id Exp
  | EUnique Hash Exp
  | EToken Id Hash
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
  | TNUVar Id -- non unique var
  | TFun Type Type
  | TUFun Type Type
  -- Data Structures
  | TPair Type Type
  | TSum Type Type
  | TLabel Id Type
  | TUnique Type
  | TToken Id
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
  | PWildcard
  | PPair Pattern Pattern
  | PLabel Id Pattern
  | PAnnot Type Pattern
  | PUnique Pattern
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Id] Type deriving (Show, Eq)

-- Ignore from here onwards
isFun :: Type -> Bool
isFun ty = case ty of
  TFun _ _ -> True
  TUFun _ _ -> True
  _ -> False

isLam :: Exp -> Bool
isLam (ELam _ _) = True
isLam (EULam _ _) = True
isLam _ = False

isUnique :: Exp -> Bool
isUnique (EUnique _ _) = True
isUnique (EToken _ _) = True
isUnique other = False

isUniqueT :: Type -> Bool
isUniqueT (TUnique _) = True
isUniqueT (TToken _) = True
isUniqueT other = False

isUniqueScheme :: Scheme -> Bool
isUniqueScheme (Scheme _ t) = isUniqueT t

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
  TNUVar var -> TVar (if var == old then new else var)
  TFun t1 t2 -> TFun (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TUFun t1 t2 -> TUFun (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TPair t1 t2 -> TPair (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TSum t1 t2 -> TSum (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TLabel id t1 -> TLabel id (renameVar t1 (old, new))
  TType x -> TType x
  TCall a b -> TCall a b
  TUnique t -> TUnique (renameVar t (old, new))
  TToken id -> TToken id

expAsPattern :: Exp -> Pattern
expAsPattern (EVar "?") = PWildcard
expAsPattern (EVar id) = PVar id
expAsPattern (ELit l) = PLit l
expAsPattern (EPair a b) = PPair (expAsPattern a) (expAsPattern b)
expAsPattern (ELabel id exp) = PLabel id (expAsPattern exp)
expAsPattern (EAnnot typ exp) = PAnnot typ (expAsPattern exp)
expAsPattern (EUnique _ a) = PUnique (expAsPattern a)
expAsPattern t@ETDef {} = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(ELam _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@(EULam _ _) = error $ "bad parse! must be var or lit on a function lhs: " ++ show t
expAsPattern t@ELet {} = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@EULet {} = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EMatch _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EApp _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(ERec _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t
expAsPattern t@(EToken _ _) = error $ "bad parse! must be var or lit on a function lhs" ++ show t

freeRefs :: Exp -> S.Set Id
freeRefs x = M.keysSet (freeRefWithCounts x)

freeRefWithCounts :: Exp -> M.Map Id Int
freeRefWithCounts (EVar id) = M.singleton id 1
freeRefWithCounts (ELam pat b) = let
    b_refs = freeRefWithCounts b
    p_refs = patFreeVars pat
  in
    M.withoutKeys b_refs p_refs
freeRefWithCounts (EULam pat b) = freeRefWithCounts (ELam pat b)
freeRefWithCounts (EApp a b) = M.unionWith (+) (freeRefWithCounts a) (freeRefWithCounts b)
freeRefWithCounts (ELet pat a b) = let
    a_refs = freeRefWithCounts a
    b_refs = freeRefWithCounts b
    p_refs = patFreeVars pat
    free_keys = S.difference (M.keysSet b_refs) p_refs
  in
    M.unionWith (+) a_refs (M.withoutKeys b_refs p_refs)
freeRefWithCounts (EULet pat a b) = let
    a_refs = freeRefWithCounts a
    b_refs = freeRefWithCounts b
    p_refs = patFreeVars pat
    free_keys = S.difference (M.keysSet b_refs) p_refs
  in
    M.unionWith (+) a_refs (M.withoutKeys b_refs p_refs)
freeRefWithCounts (EAnnot t b) = freeRefWithCounts b
freeRefWithCounts ETDef {} = M.empty
freeRefWithCounts (ELit _) = M.empty
freeRefWithCounts (ERec id x) = M.delete id $ freeRefWithCounts x
freeRefWithCounts (EMatch a b) = M.unionWith (+) (freeRefWithCounts a) (freeRefWithCounts b)
freeRefWithCounts (EPair a b) = M.unionWith (+) (freeRefWithCounts a) (freeRefWithCounts b)
freeRefWithCounts (ELabel id x) = freeRefWithCounts x
freeRefWithCounts (EUnique h x) = freeRefWithCounts x
freeRefWithCounts (EToken h x) = M.empty

patFreeVars :: Pattern -> S.Set Id
patFreeVars (PLit _) = S.empty
patFreeVars (PVar id) = S.singleton id
patFreeVars (PPair a b) = S.union (patFreeVars a) (patFreeVars b)
patFreeVars (PLabel id a) = patFreeVars a
patFreeVars (PAnnot t p) = patFreeVars p
patFreeVars (PUnique p) = patFreeVars p
patFreeVars PWildcard = S.empty