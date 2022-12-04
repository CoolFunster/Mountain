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
hasBoundUnique = foldr (\(a::Type) (b::Bool) -> b || isTUnique a) False

emptySubst :: Substitution
emptySubst = Map.empty

applySubst :: (Monad m) => Substitution -> Type -> ContextT m Type
applySubst subst ty = case ty of
  TPair a b -> TPair <$> applySubst subst a <*> applySubst subst b
  TSum a b -> TSum <$> applySubst subst a <*> applySubst subst b
  TLabel id x -> TLabel id <$> applySubst subst x
  TVar var -> return $ case Map.lookup var subst of
    Nothing -> TVar var
    Just x -> x
  TNUVar var -> case Map.lookup var subst of
    Nothing -> return $ TNUVar var
    Just (TUnique _) -> throwError BindUniqueNonUniquely
    Just x -> return x
  TFun arg res ->
    TFun <$> applySubst subst arg <*> applySubst subst res
  TInt -> return TInt
  TBool -> return TBool
  TChar -> return TChar
  TString -> return TString
  TFloat -> return TFloat
  TThing -> return TThing
  TType k -> return $ TType k
  TUnit -> return TUnit
  TCall a b -> TCall <$> applySubst subst a <*> applySubst subst b
  TUnique t -> TUnique <$> applySubst subst t
  TToken id -> return $ TToken id

applySubstOnPattern :: (Monad m) => Substitution -> Pattern -> ContextT m Pattern
applySubstOnPattern s (PLit l) = return $ PLit l
applySubstOnPattern s (PVar id) = return $ PVar id
applySubstOnPattern s (PPair a b) = PPair <$> applySubstOnPattern s a <*> applySubstOnPattern s b
applySubstOnPattern s (PLabel id b) = PLabel id <$> applySubstOnPattern s b
applySubstOnPattern s (PAnnot typ b) = PAnnot <$> applySubst s typ <*> applySubstOnPattern s b
applySubstOnPattern s (PUnique b) = PUnique <$> applySubstOnPattern s b
applySubstOnPattern s PWildcard = pure PWildcard

applySubstOnExpr :: (Monad m) => Substitution -> Exp -> ContextT m Exp
applySubstOnExpr subst t@(EVar id) = pure t
applySubstOnExpr s t@(EApp a b) = EApp <$> applySubstOnExpr s a <*> applySubstOnExpr s b
applySubstOnExpr s t@(EFun a b) = EFun <$> applySubstOnPattern s a <*> applySubstOnExpr s b
applySubstOnExpr s t@(ELet id a b) = ELet id <$> applySubstOnExpr s a <*> applySubstOnExpr s b
applySubstOnExpr s (EAnnot typ x) = EAnnot <$> applySubst s typ <*> applySubstOnExpr s x
applySubstOnExpr s (ETDef id typ x) = ETDef id <$> applySubst s typ <*> applySubstOnExpr s x
applySubstOnExpr s (ELit l) = pure $ ELit l
applySubstOnExpr s (ERec id r) = ERec id <$> applySubstOnExpr s r
applySubstOnExpr s (EMatch a b) = EMatch <$> applySubstOnExpr s a <*> applySubstOnExpr s b
applySubstOnExpr s (EPair a b) = EPair <$> applySubstOnExpr s a <*> applySubstOnExpr s b
applySubstOnExpr s (ELabel id b) = ELabel id <$> applySubstOnExpr s b
applySubstOnExpr s (EUnique h b) = EUnique h <$> applySubstOnExpr s b
applySubstOnExpr s (EToken id h) = pure $ EToken id h

applySubstScheme :: (Monad m) => Substitution -> Scheme -> ContextT m Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars <$> applySubst (foldr Map.delete subst vars) t

-- | This is much more subtle than it seems. (union is left biased)
composeSubst :: (Monad m) => Substitution -> Substitution -> ContextT m Substitution
composeSubst s1 s2 = Map.union <$> mapM (applySubst s1) s2 <*> pure s1

freeTypeVars :: Type -> Set Id
freeTypeVars ty = case ty of
 TVar var ->
   Set.singleton var
 TNUVar var ->
   Set.singleton var
 TFun t1 t2 -> Set.union (freeTypeVars t1) (freeTypeVars t2)
 TPair a b -> Set.union (freeTypeVars a) (freeTypeVars b)
 TSum a b -> Set.union (freeTypeVars a) (freeTypeVars b)
 TLabel _ b -> freeTypeVars b
 TUnique t -> freeTypeVars t
 TToken _ -> Set.empty
 TType _ -> Set.empty
 TCall a b -> Set.union (freeTypeVars a) (freeTypeVars b)
 TInt -> Set.empty
 TBool -> Set.empty
 TChar -> Set.empty
 TString -> Set.empty
 TFloat -> Set.empty
 TThing -> Set.empty
 TUnit -> Set.empty


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
  applySubst subs y
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
  final_s <- sx `composeSubst` sy
  res <- applySubst final_s (TSum resx resy)
  return (final_s, res)
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
  tmpA <- applySubst s1 r
  tmpB <- applySubst s1 r'
  (s2, tyr) <- has ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  res <- applySubst final_subs $ TFun tyl tyr
  return (final_subs, res)
has ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- has ctx a a'
  tmpA <- applySubst s1 b
  tmpB <- applySubst s1 b'
  (s2, tyB) <- has ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  res <- applySubst final_subs (TPair tyA tyB)
  return (final_subs, res)
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
      (,) res <$> applySubst res t
    Just any -> do
      resT <- instantiate any
      has ctx resT t
has ctx t b@(TVar u) = has ctx b t
has ctx (TNUVar u) t = do
  case Map.lookup u ctx of
    Nothing -> do
      res <- varBind u t
      (,) res <$> applySubst res t
    Just any -> if isEUniqueScheme any
      then throwError BindUniqueNonUniquely
      else do
        resT <- instantiate any
        has ctx resT t
has ctx t b@(TNUVar u) = has ctx b t
has ctx a@(TCall _ _) c = do
  res <- evalTCall ctx a
  has ctx res c
has ctx c a@(TCall _ _) = do
  res <- evalTCall ctx a
  has ctx c res
has ctx (TUnique x) (TUnique y) = has ctx x y
has ctx t1@(TToken id) t2@(TToken id2) =
  if id /= id2
    then throwError $ BadHas t1 t2
    else return (emptySubst, TToken id)
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
  tmpA <- applySubst s1 r
  tmpB <- applySubst s1 r'
  (s2, tyr) <- unify ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  res <- applySubst final_subs $ TFun tyl tyr
  return (final_subs, res)
unify ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- unify ctx a a'
  tmpA <- applySubst s1 b
  tmpB <- applySubst s1 b'
  (s2, tyB) <- unify ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  res <- applySubst final_subs (TPair tyA tyB)
  return (final_subs, res)
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
      (,) res <$> applySubst res t
    Just any -> do
      resT <- instantiate any
      unify ctx resT t
unify ctx a b@(TVar u) = unify ctx b a
unify ctx (TNUVar u) t = do
  case Map.lookup u ctx of
    Nothing -> do
      res <- varBind u t
      (,) res <$> applySubst res t
    Just any -> if isEUniqueScheme any
      then throwError BindUniqueNonUniquely
      else do
        resT <- instantiate any
        unify ctx resT t
unify ctx a b@(TNUVar u) = unify ctx b a
unify ctx a@(TCall _ _) c = do
  res <- evalTCall ctx a
  unify ctx res c
unify ctx c a@(TCall _ _) = do
  res <- evalTCall ctx a
  unify ctx c res
unify ctx a@(TSum x y) b@(TSum x' y') =
  trySumUnify x x' y y' `catchError` (\e -> trySumUnify x y' y x')
  where
    trySumUnify :: (Monad m) => Type -> Type -> Type -> Type -> ContextT m (Substitution, Type)
    trySumUnify x x' y y' = do
      (sub_x, typX) <- unify ctx x x'
      (sub_y, typY) <- unify ctx y y'
      final_subs <- sub_x `composeSubst` sub_y
      res <- TSum <$> applySubst final_subs typX <*> applySubst final_subs typY
      return (final_subs, res)
unify ctx a b@(TSum x' y') = unify ctx b a
unify ctx (TUnique x) (TUnique y) = unify ctx x y
unify ctx t1@(TToken id) t2@(TToken id2) =
  if id /= id2
    then throwError $ BadUnify t1 t2
    else return (emptySubst, TToken id)
unify _ t1 t2 = throwError $ BadUnify t1 t2

type Context = Map String Scheme

applySubstCtx :: (Monad m) => Substitution -> Context -> ContextT m Context
applySubstCtx subst = mapM (applySubstScheme subst)

freeTypeVarsCtx :: Context -> Set String
freeTypeVarsCtx ctx = foldMap freeTypeVarsScheme (Map.elems ctx)

generalize :: Context -> Type -> Scheme
generalize ctx t = Scheme vars t
  where
    vars = Set.toList (Set.difference (freeTypeVars t) (freeTypeVarsCtx ctx))

generalizeScheme :: Context -> Scheme -> Scheme
generalizeScheme ctx (Scheme _ exp) = generalize ctx exp

instantiate :: (Monad m) => Scheme -> ContextT m Type
instantiate (Scheme vars ty) = do
  newVars <- traverse (const (newTyVar False)) vars
  let subst = Map.fromList (zip vars newVars)
  applySubst subst ty

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

inferPattern :: (Monad m) => Bool -> Context -> Pattern -> ContextT m (Context, Type)
inferPattern isNonU _ (PLit binder) = do
  (_, tyBinder) <- inferLiteral binder
  return (Map.empty, tyBinder)
inferPattern isNonU _ (PVar binder) = do
  tyBinder <- newTyVar isNonU
  return (Map.singleton binder (Scheme [] tyBinder), tyBinder)
inferPattern isNonU ctx (PPair a b) = do
  (ctxa, tya) <- inferPattern isNonU ctx a
  (ctxb, tyb) <- inferPattern isNonU ctx b
  return (Map.union ctxa ctxb, TPair tya tyb)
inferPattern isNonU ctx (PLabel id x) = do
  (new_ctx, typ) <- inferPattern isNonU ctx x
  return (new_ctx, TLabel id typ)
inferPattern isNonU ctx (PAnnot typ x) = do
  (s1, typx) <- inferPattern isNonU ctx x
  (s2, final_typ) <- has ctx typ typx
  res <- applySubstCtx s2 s1
  return (res, final_typ)
inferPattern isNonU ctx (PUnique x) = do
  (s1, typx) <- inferPattern isNonU ctx x
  return (s1, TUnique typx)
inferPattern isNonU ctx PWildcard = do
  res <- newTyVar False
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
    (s2, tyArg) <- applySubstCtx s1 ctx >>= infer <*> pure arg
    tmp <- applySubst s2 (inputTyp tyFun)
    (s3, input_typ) <- has ctx tmp tyArg
    final_subst <- s3 `composeSubst` s2 >>= composeSubst s1
    res <- applySubst final_subst tyFun
    return (final_subst, outputTyp res)
    where
      inputTyp (TFun a b) = a
      outputTyp (TFun a b) = b
  EFun pattern body -> do
    (ctx_p, tyP) <- inferPattern False ctx pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    res <- applySubst s1 tyP
    return (s1, TFun res tyBody)
  m@(EMatch a b) -> do
    foo_type <- TFun <$> newTyVar False <*> newTyVar False
    (s1, tyA) <- infer ctx a
    if not $ isTFun tyA
      then err
    else do
      res <- applySubstCtx s1 ctx
      (s2, tyB) <- infer res b
      if not $ isTFun tyB
        then err
      else do
        res2 <- applySubst s2 tyA
        (final_subs, final_ty) <- unify ctx res2 tyB
        case final_ty of
          TFun _ _ -> return (final_subs, final_ty)
          other -> err
        `catchError` (\e -> do {
              res3 <- applySubst s2 tyA
            ; let (u1, ax, ay) = tFunArgs res3
            ; let (u2, bx, by) = tFunArgs tyB
            ; (subs_x, tyX) <- unifyJoin ax bx
            ; (subs_y, tyY) <- unifyJoin ay by
            ; res_subs <- subs_x `composeSubst` subs_y
            ; if isTUnique tyX || isTUnique tyY
                then return (res_subs, TUnique (TFun tyX tyY))
                else return (res_subs, TFun tyX tyY)
          })
    where
      err = throwError $ MatchWithNonFunction m
      unifyJoin a b = unify ctx a b `catchError` (\e -> return (emptySubst, TSum a b))
      tFunArgs (TFun a b) = (False,a,b)
  ELet pattern binding body -> do
    (s1, tyBinder) <- infer ctx binding
    (ctx_p, tyP) <- inferPattern False ctx pattern
    let newCtx = Map.union ctx_p ctx
    res <- applySubst s1 tyBinder
    (s2, typB) <- unify newCtx tyP res
    tmpA <- s2 `composeSubst` s1
    res2 <- applySubstCtx tmpA ctx_p
    let patCtx = Map.map (generalizeScheme ctx) res2
    finalCtx <- applySubstCtx tmpA $ Map.union patCtx ctx
    (s3, tyBody) <- infer finalCtx body
    (,) <$> s3 `composeSubst` tmpA <*> pure tyBody
  EPair a b -> do
    (s1, tyA) <- infer ctx a
    tmp <- applySubstCtx s1 ctx
    (s2, tyB) <- infer tmp b
    res <- s2 `composeSubst` s1
    subs <- applySubst s2 tyA
    if isTUnique tyA || isTUnique tyB
      then return (res, TUnique $ TPair subs tyB)
      else return (res, TPair subs tyB)
  ELabel id x -> do
    (s1, typ) <- infer ctx x
    if isTUnique typ
      then return (s1, TUnique $ TLabel id typ)
      else return (s1, TLabel id typ)
  EAnnot typ x -> do
    (s1, typx) <- infer ctx x
    (fs, u_type) <- has ctx typ typx
    res <- fs `composeSubst` s1
    if isTUnique typ || isTUnique u_type
      then return (res, TUnique u_type)
      else return (res, u_type)
  ERec id (EAnnot typ x) -> do
    let scheme = Scheme [] typ
    let tmpCtx = Map.insert id scheme ctx
    (subs, x) <- infer tmpCtx x
    if isTUnique x
      then throwError $ ThisShouldBeNotUnique exp
      else return (subs, x)
  ERec id other -> throwError $ RecursiveWithNoTypeAnnotation other
  ETDef id t rest -> do
    let new_ctx = Map.insert id (Scheme [] t) ctx
    applySubstOnExpr (Map.singleton id t) rest >>= infer new_ctx
  EUnique h x -> do
    (subst, typX) <- infer ctx x
    case typX of
      TUnique _ -> return (subst, typX)
      other -> return (subst, TUnique typX)
  EToken id h -> return (emptySubst, TToken id)

typeInference :: (Monad m) => Context -> Exp -> ContextT m Scheme
typeInference ctx exp = do
  (s, t) <- infer ctx exp
  applySubstScheme s (generalize ctx t)

primitives :: Context
primitives = Map.empty