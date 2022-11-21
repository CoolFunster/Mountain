{-# LANGUAGE ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.Typechecker where

import CopiedMountain.PrettyPrinter
import CopiedMountain.Data.Errors

import Control.Monad (replicateM)
import Control.Monad.State (State, runState, get, put, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError (catchError))
import Data.Maybe (fromMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import CopiedMountain.Data.AST

import Debug.Trace
import qualified Data.Text as T
import CopiedMountain.Context

type Substitution = Map String Type

hasBoundUnique :: Substitution -> Bool
hasBoundUnique = foldr (\(a::Type) (b::Bool) -> b || isUniqueT a) False

emptySubst :: Substitution
emptySubst = Map.empty

applySubst :: Substitution -> Type -> Type
applySubst subst ty = case ty of
  TPair a b -> TPair (applySubst subst a) (applySubst subst b)
  TSum a b -> TSum (applySubst subst a) (applySubst subst b)
  TLabel id x -> TLabel id (applySubst subst x)
  TVar var -> case Map.lookup var subst of
    Nothing -> TVar var
    Just x -> x
  TFun arg res ->
    TFun (applySubst subst arg) (applySubst subst res)
  TUFun arg res ->
    TUFun (applySubst subst arg) (applySubst subst res)
  TInt -> TInt
  TBool -> TBool
  TChar -> TChar
  TString -> TString
  TFloat -> TFloat
  TThing -> TThing
  TType k -> TType k
  TUnit -> TUnit
  TCall a b -> TCall (applySubst subst a) (applySubst subst b)
  TUnique t -> TUnique (applySubst subst t)

applySubstOnPattern :: Substitution -> Pattern -> Pattern
applySubstOnPattern s (PLit l) = PLit l
applySubstOnPattern s (PVar id) = PVar id
applySubstOnPattern s (PPair a b) = PPair (applySubstOnPattern s a) (applySubstOnPattern s b)
applySubstOnPattern s (PLabel id b) = PLabel id (applySubstOnPattern s b)
applySubstOnPattern s (PAnnot typ b) = PAnnot (applySubst s typ) (applySubstOnPattern s b)
applySubstOnPattern s (PUnique b) = PUnique (applySubstOnPattern s b)

applySubstOnExpr :: Substitution -> Exp -> Exp
applySubstOnExpr subst t@(EVar id) = t
applySubstOnExpr s t@(EApp a b) = EApp (applySubstOnExpr s a) (applySubstOnExpr s b)
applySubstOnExpr s t@(ELam a b) = ELam (applySubstOnPattern s a) (applySubstOnExpr s b)
applySubstOnExpr s t@(EULam a b) = EULam (applySubstOnPattern s a) (applySubstOnExpr s b)
applySubstOnExpr s t@(ELet id a b) = ELet id (applySubstOnExpr s a) (applySubstOnExpr s b)
applySubstOnExpr s t@(EULet id a b) = EULet id (applySubstOnExpr s a) (applySubstOnExpr s b)
applySubstOnExpr s (EAnnot typ x) = EAnnot (applySubst s typ) (applySubstOnExpr s x)
applySubstOnExpr s (ETDef id typ x) = ETDef id (applySubst s typ) (applySubstOnExpr s x)
applySubstOnExpr s (ELit l) = ELit l
applySubstOnExpr s (ERec id r) = ERec id (applySubstOnExpr s r)
applySubstOnExpr s (EMatch a b) = EMatch (applySubstOnExpr s a) (applySubstOnExpr s b)
applySubstOnExpr s (EPair a b) = EPair (applySubstOnExpr s a) (applySubstOnExpr s b)
applySubstOnExpr s (ELabel id b) = ELabel id (applySubstOnExpr s b)
applySubstOnExpr s (EUnique h b) = EUnique h (applySubstOnExpr s b)

applySubstScheme :: Substitution -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst (foldr Map.delete subst vars) t)

-- | This is much more subtle than it seems. (union is left biased)
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1

freeTypeVars :: Type -> Set Id
freeTypeVars ty = case ty of
 TVar var ->
   Set.singleton var
 TFun t1 t2 ->
   Set.union (freeTypeVars t1) (freeTypeVars t2)
 _ ->
   Set.empty

freeTypeVarsScheme :: Scheme -> Set Id
freeTypeVarsScheme (Scheme vars t) =
  Set.difference (freeTypeVars t) (Set.fromList vars)

-- | Creates a fresh unification variable and binds it to the given type
varBind :: (Monad m) => String -> Type -> ContextT m Substitution
varBind var ty
  | ty == TVar var = return emptySubst
  | var `Set.member` freeTypeVars ty = throwError $ OccursCheck var ty
  | otherwise = return (Map.singleton var ty)

evalTCall :: (Monad m) => Context -> Type -> ContextT m Type
evalTCall ctx (TCall (TVar id) b) = do
  case Map.lookup id ctx of
    Nothing -> throwError $ UnboundId id
    Just any -> do
      res <- instantiate any
      evalTCall ctx (TCall res b)
evalTCall ctx (TCall (TFun x y) b) = do
  (subs, typX) <- has ctx x b
  return $ applySubst subs y
evalTCall ctx (TCall (TUFun x y) b) = do
  (subs, typX) <- has ctx x b
  return $ applySubst subs y
evalTCall ctx (TCall somethin b) = error $ "what is this case?: " ++ show somethin
evalTCall _ _ = error "Must Be TCall"

has :: (Monad m) => Context -> Type -> Type -> ContextT m (Substitution, Type)
has _ TUnit TUnit = return (emptySubst, TUnit)
has _ TInt TInt = return (emptySubst, TInt)
has _ TBool TBool = return (emptySubst, TBool)
has _ TChar TChar = return (emptySubst, TChar)
has _ TString TString = return (emptySubst, TString)
has _ TFloat TFloat = return (emptySubst, TFloat)
has _ TThing TThing = return (emptySubst, TThing)
has ctx a b@(TSum x y) = do
  (sx, resx) <- has ctx a x
  (sy, resy) <- has ctx a y
  let final_s = sx `composeSubst` sy
  return (final_s, applySubst final_s (TSum resx resy))
has ctx a@(TSum x y) b = do
  res <- tryHas x b
  case res of
    Nothing -> do
      res2 <- tryHas y b
      case res2 of
        Just x -> return x
        Nothing -> throwError $ BadHas a b
    Just x -> return x
  where
    tryHas :: (Monad m) => Type -> Type -> ContextT m (Maybe (Substitution, Type))
    tryHas a b = (Just <$> has ctx a b) `catchError` (\e -> return Nothing)
has ctx (TFun l r) (TFun l' r') = do
  (s1, tyl) <- has ctx l l'
  (s2, tyr) <- has ctx (applySubst s1 r) (applySubst s1 r')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs $ TFun tyl tyr)
has ctx (TFun l r) (TUFun l' r') = has ctx (TFun l r) (TFun l' r')
has ctx a@(TUFun l r) b@(TFun l' r') = throwError $ BadHas a b
has ctx (TUFun l r) (TUFun l' r') = do
  (s1, tyl) <- has ctx l l'
  (s2, tyr) <- has ctx (applySubst s1 r) (applySubst s1 r')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs $ TUFun tyl tyr)
has ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- has ctx a a'
  (s2, tyB) <- has ctx (applySubst s1 b) (applySubst s1 b')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs (TPair tyA tyB))
has ctx a@(TLabel id x) b@(TLabel id2 y) = do
  if id == id2
    then do
      (ctx, ty) <- has ctx x y
      return (ctx, TLabel id ty)
    else throwError $ BadHas a b
has ctx (TVar u) t = do
  case Map.lookup u ctx of
    Nothing -> do
      res <- varBind u t
      return (res, applySubst res t)
    Just any -> do
      resT <- instantiate any
      has ctx resT t
has ctx t b@(TVar u) = has ctx b t
has ctx a@(TCall _ _) c = do
  res <- evalTCall ctx a
  has ctx res c
has ctx c a@(TCall _ _) = do
  res <- evalTCall ctx a
  has ctx c res
has ctx (TUnique x) (TUnique y) = has ctx x y
has _ t1 t2 = throwError $ BadHas t1 t2


unify :: (Monad m) => Context -> Type -> Type -> ContextT m (Substitution, Type)
unify _ TUnit TUnit = return (emptySubst, TUnit)
unify _ TInt TInt = return (emptySubst, TInt)
unify _ TBool TBool = return (emptySubst, TBool)
unify _ TChar TChar = return (emptySubst, TChar)
unify _ TString TString = return (emptySubst, TString)
unify _ TFloat TFloat = return (emptySubst, TFloat)
unify _ TThing TThing = return (emptySubst, TThing)
unify ctx (TFun l r) (TFun l' r') = do
  (s1, tyl) <- unify ctx l l'
  (s2, tyr) <- unify ctx (applySubst s1 r) (applySubst s1 r')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs $ TFun tyl tyr)
unify ctx (TFun l r) (TUFun l' r') = unify ctx (TFun l r) (TFun l' r')
unify ctx (TUFun l r) (TFun l' r') = unify ctx (TFun l r) (TFun l' r')
unify ctx (TUFun l r) (TUFun l' r') = do
  (s, typ) <- unify ctx (TFun l r) (TFun l' r')
  let TFun l'' r'' = typ
  return (s, TUFun l'' r'')
unify ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- unify ctx a a'
  (s2, tyB) <- unify ctx (applySubst s1 b) (applySubst s1 b')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs (TPair tyA tyB))
unify ctx a@(TLabel id x) b@(TLabel id2 y) = do
  if id == id2
    then do
      (ctx, ty) <- unify ctx x y
      return (ctx, TLabel id ty)
    else throwError $ BadUnify a b
unify ctx (TVar u) t = do
  case Map.lookup u ctx of
    Nothing -> do
      res <- varBind u t
      return (res, applySubst res t)
    Just any -> do
      resT <- instantiate any
      unify ctx resT t
unify ctx a b@(TVar u) = unify ctx b a
unify ctx a@(TCall _ _) c = do
  res <- evalTCall ctx a
  unify ctx res c
unify ctx c a@(TCall _ _) = do
  res <- evalTCall ctx a
  unify ctx c res
unify ctx a@(TSum x y) b@(TSum x' y') = do
  (sub_x, typX) <- unify ctx x x'
  (sub_y, typY) <- unify ctx y y'
  let final_subs = sub_x `composeSubst` sub_y
  return (final_subs, TSum (applySubst final_subs typX) (applySubst final_subs typY))
  `catchError` (\e -> do {
     (sub_x, typX) <- unify ctx x y'
    ;(sub_y, typY) <- unify ctx y x'
    ;let final_subs = sub_x `composeSubst` sub_y
    ;return (final_subs, TSum (applySubst final_subs typX) (applySubst final_subs typY))
  })
unify ctx a b@(TSum x' y') = unify ctx b a
unify ctx (TUnique x) (TUnique y) = unify ctx x y
unify _ t1 t2 = throwError $ BadUnify t1 t2

type Context = Map String Scheme

applySubstCtx :: Substitution -> Context -> Context
applySubstCtx subst = Map.map (applySubstScheme subst)

freeTypeVarsCtx :: Context -> Set String
freeTypeVarsCtx ctx = foldMap freeTypeVarsScheme (Map.elems ctx)

generalize :: Context -> Type -> Scheme
generalize ctx t = Scheme vars t
  where
    vars = Set.toList (Set.difference (freeTypeVars t) (freeTypeVarsCtx ctx))

instantiate :: (Monad m) => Scheme -> ContextT m Type
instantiate (Scheme vars ty) = do
  newVars <- traverse (const newTyVar) vars
  let subst = Map.fromList (zip vars newVars)
  return (applySubst subst ty)

inferLiteral :: (Monad m) => Lit -> ContextT m (Substitution, Type)
inferLiteral lit =
  return (emptySubst, case lit of
    LInt _ -> TInt
    LBool _ -> TBool
    LThing _ -> TThing
    LChar _ -> TChar
    LString _ -> TString
    LFloat _ -> TFloat
    LUnit -> TUnit)

inferPattern :: (Monad m) => Context -> Pattern -> ContextT m (Context, Type)
inferPattern _ (PLit binder) = do
  (_, tyBinder) <- inferLiteral binder
  return (Map.empty, tyBinder)
inferPattern _ (PVar binder) = do
  tyBinder <- newTyVar
  return (Map.singleton binder (Scheme [] tyBinder), tyBinder)
inferPattern ctx (PPair a b) = do
  (ctxa, tya) <- inferPattern ctx a
  (ctxb, tyb) <- inferPattern ctx b
  return (Map.union ctxa ctxb, TPair tya tyb)
inferPattern ctx (PLabel id x) = do
  (new_ctx, typ) <- inferPattern ctx x
  return (new_ctx, TLabel id typ)
inferPattern ctx (PAnnot typ x) = do
  (s1, typx) <- inferPattern ctx x
  (_, final_typ) <- has ctx typ typx
  return (s1, final_typ)
inferPattern ctx (PUnique x) = do
  (s1, typx) <- inferPattern ctx x
  return (s1, TUnique typx)
inferPattern ctx PWildcard = do
  res <- newTyVar
  return (ctx, res)
  

infer :: (Monad m) => Context -> Exp -> ContextT m (Substitution, Type)
infer ctx exp = case exp of
  EVar var -> case Map.lookup var ctx of
    Nothing -> throwError $ UnboundId var
    Just scheme -> do
      ty <- instantiate scheme
      return (emptySubst, ty)
  ELit lit ->
    inferLiteral lit
  EApp fun arg -> do
    (s1, tyFun) <- infer ctx fun
    (s2, tyArg) <- infer (applySubstCtx s1 ctx) arg
    (s3, input_typ) <- has ctx (applySubst s2 (inputTyp tyFun)) tyArg
    let final_subst = s3 `composeSubst` s2 `composeSubst` s1
    if hasBoundUnique s3
      then case tyFun of
        TFun _ _ -> throwError $ BindUniqueNonUniquely exp
        TUFun _ _ -> return (final_subst, TUnique $ outputTyp (applySubst final_subst tyFun))
        other -> error "what is this case"
      else return (final_subst, outputTyp (applySubst final_subst tyFun))
    where
      inputTyp (TFun a b) = a
      inputTyp (TUFun a b) = a
      outputTyp (TFun a b) = b
      outputTyp (TUFun a b) = b
  ELam p@(PUnique x) b@body -> do
    (subs, typ) <- infer ctx (ELam x b)
    let TFun a b = typ
    return (subs, TFun (TUnique a) b)
  ELam pattern body -> do
    (ctx_p, tyP) <- inferPattern ctx pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    return (s1, TFun (applySubst s1 tyP) tyBody)
  EULam pattern body -> do
    (ctx_p, tyP) <- inferPattern ctx pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    return (s1, TUFun (applySubst s1 tyP) tyBody)
  m@(EMatch a b) -> do
    foo_type <- TFun <$> newTyVar <*> newTyVar
    (s1, tyA) <- infer ctx a
    if not $ isFun tyA
      then err
    else do
      (s2, tyB) <- infer (applySubstCtx s1 ctx) b
      if not $ isFun tyB
        then err
      else do
        (final_subs, final_ty) <- unify ctx (applySubst s2 tyA) tyB
        case final_ty of
          TFun _ _ -> return (final_subs, final_ty)
          other -> err
        `catchError` (\e -> do {
              let (u1, ax, ay) = tFunArgs $ applySubst s2 tyA
            ; let (u2, bx, by) = tFunArgs tyB
            ; (subs_x, tyX) <- unifyJoin ax bx
            ; (subs_y, tyY) <- unifyJoin ay by
            ; if isUniqueT tyX || isUniqueT tyY
                then return (subs_x `composeSubst` subs_y, if u1 && u2 then TUnique (TUFun tyX tyY) else TUnique (TFun tyX tyY))
                else return (subs_x `composeSubst` subs_y, if u1 && u2 then TUFun tyX tyY else TFun tyX tyY)
          })
    where
      err = throwError $ MatchWithNonFunction m
      unifyJoin a b = unify ctx a b `catchError` (\e -> return (emptySubst, TSum a b))
      tFunArgs (TFun a b) = (False,a,b)
      tFunArgs (TUFun a b) = (True,a,b)
  ELet pattern binding body -> do
    (s1, tyBinder) <- infer ctx binding
    (ctx_p, tyP) <- inferPattern ctx pattern
    let newCtx = Map.union ctx_p ctx
    (s2, typB) <- unify newCtx tyP (applySubst s1 tyBinder)
    if hasBoundUnique s2
      then throwError $ BindUniqueNonUniquely exp
      else do
        (s3, tyBody) <- infer (applySubstCtx (s2 `composeSubst` s1) newCtx) body
        return (s3 `composeSubst` s2 `composeSubst` s1, tyBody)
  EULet pattern binding body -> do
    (s1, tyBinder) <- infer ctx binding
    (ctx_p, tyP) <- inferPattern ctx pattern
    let newCtx = Map.union ctx_p ctx
    (s2, typB) <- unify newCtx tyP (applySubst s1 tyBinder)
    (s3, tyBody) <- infer (applySubstCtx (s2 `composeSubst` s1) newCtx) body
    return (s3 `composeSubst` s2 `composeSubst` s1, tyBody)
  EPair a b -> do
    (s1, tyA) <- infer ctx a
    (s2, tyB) <- infer (applySubstCtx s1 ctx) b
    if isUniqueT tyA || isUniqueT tyB
      then return (s2 `composeSubst` s1, TUnique $ TPair (applySubst s2 tyA) tyB)
      else return (s2 `composeSubst` s1, TPair (applySubst s2 tyA) tyB)
  ELabel id x -> do
    (s1, typ) <- infer ctx x
    if isUniqueT typ
      then return (s1, TUnique $ TLabel id typ)
      else return (s1, TLabel id typ)
  EAnnot typ x -> do
    (s1, typx) <- infer ctx x
    (fs, u_type) <- has ctx typ typx
    if isUniqueT typ || isUniqueT u_type
      then return (fs `composeSubst` s1, TUnique $ u_type)
      else return (fs `composeSubst` s1, u_type)
  ERec id (EAnnot typ x) -> do
    let scheme = Scheme [] typ
    let tmpCtx = Map.insert id scheme ctx
    (subs, x) <- infer tmpCtx x
    if isUniqueT x
      then throwError $ ThisShouldBeNotUnique exp
      else return (subs, x)
  ERec id other -> throwError $ RecursiveWithNoTypeAnnotation other
  ETDef id t rest -> do
    let new_ctx = Map.insert id (Scheme [] t) ctx
    infer new_ctx $ applySubstOnExpr (Map.singleton id t) rest
  EUnique h x -> do
    (subst, typX) <- infer ctx x
    case typX of
      TUnique _ -> return (subst, typX)
      other -> return (subst, TUnique typX)

typeInference :: (Monad m) => Context -> Exp -> ContextT m Type
typeInference ctx exp = do
  (s, t) <- infer ctx exp
  return (applySubst s t)

primitives :: Context
primitives = Map.fromList
  [ ("identity", Scheme ["a"] (TFun (TVar "a") (TVar "a")))
  , ("const", Scheme ["a", "b"] (TFun (TVar "a") (TFun (TVar "b") (TVar "a"))))
  , ("add", Scheme [] (TFun TInt (TFun TInt TInt)))
  , ("gte", Scheme [] (TFun TInt (TFun TInt TBool)))
  , ("if", Scheme ["a"] (TFun TBool (TFun (TVar "a") (TFun (TVar "a") (TVar "a")))))
  ]