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
  | EFun Pattern Exp
  | EApp Exp Exp
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
  | TFun (Usage, Type) Type
  -- Data Structures
  | TPair Type Type
  | TSum Type Type
  | TLabel Id Type
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
  | PAnnot (Usage,Type) Pattern
  deriving (Eq, Ord, Show)

data UseCount
  =
    CSingle
  | CMany
  | CAny
  deriving (Show, Ord, Eq)

-- The type of patterns
data Usage =
    ULit UseCount
  | UPair UseCount Usage Usage
  | USum Usage Usage
  deriving (Show, Ord, Eq)

data Scheme = Scheme [Id] Type deriving (Show, Eq)

-- Ignore from here onwards
isTFun :: Type -> Bool
isTFun ty = case ty of
  TFun _ _ -> True
  _ -> False

isEFun :: Exp -> Bool
isEFun (EFun _ _) = True
isEFun _ = False

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
  TFun (u,t) t2 -> TFun (u,renameVar t (old, new)) (renameVar t2 (old, new))
  TPair t1 t2 -> TPair (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TSum t1 t2 -> TSum (renameVar t1 (old, new)) (renameVar t2 (old, new))
  TLabel id t1 -> TLabel id (renameVar t1 (old, new))
  TType x -> TType x
  TCall a b -> TCall a b
  TToken id -> TToken id

freeRefs :: Exp -> S.Set Id
freeRefs x = M.keysSet (freeRefWithCounts x)

copiedFreeRef :: Exp -> S.Set Id
copiedFreeRef e = M.keysSet $ M.filter (> 1) $ freeRefWithCounts e

freeRefWithCounts :: Exp -> M.Map Id Int
freeRefWithCounts (EVar id) = M.singleton id 1
freeRefWithCounts (EFun pat b) = let
    b_refs = freeRefWithCounts b
    p_refs = patFreeVars pat
  in
    M.withoutKeys b_refs p_refs
freeRefWithCounts (EApp a b) = M.unionWith (+) (freeRefWithCounts a) (freeRefWithCounts b)
freeRefWithCounts (ELet pat a b) = let
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
freeRefWithCounts (EToken h x) = M.empty

patFreeVars :: Pattern -> S.Set Id
patFreeVars (PLit _) = S.empty
patFreeVars (PVar id) = S.singleton id
patFreeVars (PPair a b) = S.union (patFreeVars a) (patFreeVars b)
patFreeVars (PLabel id a) = patFreeVars a
patFreeVars (PAnnot t p) = patFreeVars p
patFreeVars PWildcard = S.empty

getUseCount :: Usage -> UseCount
getUseCount (ULit c) = c
getUseCount (UPair c _ _) = c
getUseCount (USum a b) = minimum [getUseCount a, getUseCount b]

useCountPair :: [UseCount] -> UseCount
useCountPair counts 
  | CSingle `elem` counts = CSingle
  | CAny `elem` counts = CAny
  | otherwise = CMany

updateUsage :: Usage -> Usage
updateUsage (ULit c) = ULit c
updateUsage (UPair c a b) = do
  let a' = updateUsage a
  let b' = updateUsage b
  let usages = c:map getUseCount [a,b]
  UPair (minimum usages) a' b'
updateUsage (USum a b) = USum (updateUsage a) (updateUsage b)
