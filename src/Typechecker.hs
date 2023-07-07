{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# language OverloadedStrings #-}
module Typechecker where

import PrettyPrinter
import Errors

import Control.Monad (replicateM, foldM)
import Control.Monad.State (State, runState, get, put, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError (catchError))
import Data.Maybe (fromMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import AST

import Debug.Trace
import qualified Data.Text as T
import Context
import qualified Data.Set as S
import Data.Foldable (foldrM)
import Data.List (groupBy)

type Substitution a = Map String a

emptySubst :: Substitution Type
emptySubst = Map.empty

applySubst :: (Monad m) => Substitution Type -> Type -> ContextT m Type
applySubst subst ty = do
  case ty of
    TPair a b -> TPair <$> applySubst subst a <*> applySubst subst b
    TSum a b -> TSum <$> applySubst subst a <*> applySubst subst b
    TLabel id x -> TLabel id <$> applySubst subst x
    TVar var -> return $ case Map.lookup var subst of
      Nothing -> TVar var
      Just x -> x
    TFun arg res -> do
      arg' <- applySubst subst arg
      TFun arg' <$> applySubst subst res
    TInt -> return TInt
    TBool -> return TBool
    TChar -> return TChar
    TString -> return TString
    TFloat -> return TFloat
    TThing -> return TThing
    TType k -> return $ TType k
    TUnit -> return TUnit
    TCall a b -> TCall <$> applySubst subst a <*> applySubst subst b
    TToken id -> return $ TToken id
    TUsage u t -> TUsage u <$> applySubst subst t
    TInterface stmts -> TInterface <$> mapM applySubstModuleStmt stmts
      where
        applySubstModuleStmt :: (Monad m) => ModuleStmt -> ContextT m ModuleStmt
        applySubstModuleStmt stmt = case stmt of
          MKind s ki -> return $ MKind s ki
          d@(MType _ _) -> throwError $ ConcreteTypeOrExprInInterface d
          MDecl s ty' -> MDecl s <$> applySubst subst ty'
          d@(MData _ _) -> throwError $ ConcreteTypeOrExprInInterface d

applySubstOnPattern :: (Monad m) => Substitution Type -> Pattern -> ContextT m Pattern
applySubstOnPattern s (PLit l) = return $ PLit l
applySubstOnPattern s (PVar id) = return $ PVar id
applySubstOnPattern s (PPair a b) = PPair <$> applySubstOnPattern s a <*> applySubstOnPattern s b
applySubstOnPattern s (PLabel id b) = PLabel id <$> applySubstOnPattern s b
applySubstOnPattern s (PAnnot typ b) = do
  res <- applySubst s typ
  PAnnot res <$> applySubstOnPattern s b
applySubstOnPattern s PWildcard = pure PWildcard

applySubstOnExpr :: (Monad m) => Substitution Type -> Exp -> ContextT m Exp
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
applySubstOnExpr s (EToken id h) = pure $ EToken id h
applySubstOnExpr s (EModule stmts) = EModule <$> mapM applySubstOnExprModStmt stmts
  where
    applySubstOnExprModStmt :: (Monad m) => ModuleStmt -> ContextT m ModuleStmt
    applySubstOnExprModStmt stmt = case stmt of
      MData str exp -> MData str <$> applySubstOnExpr s exp
      other -> return other

applySubstScheme :: (Monad m) => Substitution Type -> Scheme -> ContextT m Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars <$> applySubst (foldr Map.delete subst vars) t

-- | This is much more subtle than it seems. (union is left biased)
composeSubst :: (Monad m) => Substitution Type -> Substitution Type -> ContextT m (Substitution Type)
composeSubst s1 s2 = Map.union <$> mapM (applySubst s1) s2 <*> pure s1

freeTypeVars :: Type -> Set Id
freeTypeVars ty = case ty of
 TVar var ->
   Set.singleton var
 TFun t1 t2 -> Set.union (freeTypeVars t1) (freeTypeVars t2)
 TPair a b -> Set.union (freeTypeVars a) (freeTypeVars b)
 TSum a b -> Set.union (freeTypeVars a) (freeTypeVars b)
 TLabel _ b -> freeTypeVars b
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
 TUsage _ t -> freeTypeVars t
 TInterface stmts -> foldr (Set.union . freeTypeVarStmts) Set.empty stmts
  where
    freeTypeVarStmts :: ModuleStmt -> Set Id
    freeTypeVarStmts stmt = case stmt of
      MKind s ki -> S.empty
      MType s ty' -> freeTypeVars ty'
      MDecl s ty' -> freeTypeVars ty'
      MData s exp -> S.empty


freeTypeVarsScheme :: Scheme -> Set Id
freeTypeVarsScheme (Scheme vars t) =
  Set.difference (freeTypeVars t) (Set.fromList vars)

-- | Creates a fresh unification variable and binds it to the given type
varBind :: (Monad m) => String -> Type -> ContextT m (Substitution Type)
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

has :: (Monad m) => Context -> Type -> Type -> ContextT m (Substitution Type, Type)
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
    tryHas :: (Monad m) => Type -> Type -> ContextT m (Maybe (Substitution Type, Type))
    tryHas a b = (Just <$> has ctx a b) `catchError` (\e -> return Nothing)
has ctx (TFun l r) (TFun l' r') = do
  (s1, tyl) <- has ctx l l'
  tmpA <- applySubst s1 r
  tmpB <- applySubst s1 r'
  (s2, tyr) <- has ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  tfun_input <- applySubst final_subs l
  res <- applySubst final_subs $ TFun tfun_input tyr
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
has ctx a@(TCall _ _) c = do
  res <- evalTCall ctx a
  has ctx res c
has ctx c a@(TCall _ _) = do
  res <- evalTCall ctx a
  has ctx c res
has ctx t1@(TToken id) t2@(TToken id2) =
  if id /= id2
    then throwError $ BadHas t1 t2
    else return (emptySubst, TToken id)
has ctx (TUsage u1 t1) (TUsage u2 t2) = do
  (subst, typ) <- has ctx t1 t2
  usage <- hasUseCount u1 u2
  return (subst, TUsage usage typ)
has ctx (TUsage u1 t1) t2 = has ctx (TUsage u1 t1) (TUsage CAny t2)
has ctx t1 (TUsage u2 t2)  = has ctx (TUsage CAny t1) (TUsage u2 t2)
has _ t1 t2 = throwError $ BadHas t1 t2


unify :: (Monad m) => Context -> Type -> Type -> ContextT m (Substitution Type, Type)
unify ctx t1 t2 = case (t1, t2) of
  (TUnit, TUnit) -> return (emptySubst, TUnit)
  (TInt, TInt) -> return (emptySubst, TInt)
  (TBool, TBool) -> return (emptySubst, TBool)
  (TChar, TChar) -> return (emptySubst, TChar)
  (TString, TString) -> return (emptySubst, TString)
  (TFloat, TFloat) -> return (emptySubst, TFloat)
  (TThing, TThing) -> return (emptySubst, TThing)
  (TUnit, t2') -> throwError $ BadUnify TUnit t2'
  (TInt, t2') -> throwError $ BadUnify TInt t2'
  (TBool, t2') -> throwError $ BadUnify TBool t2'
  (TChar, t2') -> throwError $ BadUnify TChar t2'
  (TString, t2') -> throwError $ BadUnify TString t2'
  (TFloat, t2') -> throwError $ BadUnify TFloat t2'
  (TThing, t2') -> throwError $ BadUnify TThing t2'
  (TFun l r, TFun l' r') -> do
    (s1, tyl) <- unify ctx l l'
    tmpA <- applySubst s1 r
    tmpB <- applySubst s1 r'
    (s2, tyr) <- unify ctx tmpA tmpB
    final_subs <- s2 `composeSubst` s1
    tfun_input <- applySubst final_subs l
    res <- applySubst final_subs $ TFun tfun_input tyr
    return (final_subs, res)
  (t1'@(TFun _ _), t2') -> throwError $ BadUnify t1' t2'
  (TPair a b, TPair a' b') -> do
    (s1, tyA) <- unify ctx a a'
    tmpA <- applySubst s1 b
    tmpB <- applySubst s1 b'
    (s2, tyB) <- unify ctx tmpA tmpB
    final_subs <- s2 `composeSubst` s1
    res <- applySubst final_subs (TPair tyA tyB)
    return (final_subs, res)
  (t1'@(TPair _ _), t2') -> throwError $ BadUnify t1' t2
  (TLabel id x, TLabel id2 y) ->
    if id == id2
      then do
        (ctx', ty) <- unify ctx x y
        return (ctx', TLabel id ty)
      else throwError $ BadUnify t1 t2
  (t1'@(TLabel _ _), t2') -> throwError $ BadUnify t1' t2'
  (TVar u, t) -> do
    case Map.lookup u ctx of
      Nothing -> do
        res <- varBind u t
        (,) res <$> applySubst res t
      Just any -> do
        resT <- instantiate any
        unify ctx resT t
  (TCall _ _, t) -> do
    res <- evalTCall ctx t1
    unify ctx res t
  (TSum x y, TSum x' y') ->
    trySumUnify x x' y y' `catchError` (\e -> trySumUnify x y' y x')
    where
      trySumUnify :: (Monad m) => Type -> Type -> Type -> Type -> ContextT m (Substitution Type, Type)
      trySumUnify x x' y y' = do
        (sub_x, typX) <- unify ctx x x'
        (sub_y, typY) <- unify ctx y y'
        final_subs <- sub_x `composeSubst` sub_y
        res <- TSum <$> applySubst final_subs typX <*> applySubst final_subs typY
        return (final_subs, res)
  (t1'@(TSum _ _), t2') -> throwError $ BadUnify t1' t2'
  (TToken id, TToken id2) ->
    if id /= id2
      then throwError $ BadUnify t1 t2
      else return (emptySubst, TToken id)
  (t1'@(TToken _), t2') -> throwError $ BadUnify t1' t2
  (TUsage u1 t1, TUsage u2 t2) -> do
    (subst, typ) <- unify ctx t1 t2
    usage <- unifyUseCount u1 u2
    return (subst, TUsage usage typ)
  (TUsage u1 t1, t2) -> do
    usage_t2 <- inferUseCount t2
    unify ctx (TUsage u1 t1) (TUsage usage_t2 t2)
  (t1, TUsage u2 t2) -> unify ctx (TUsage CAny t1) (TUsage u2 t2)
  (TType k, TType k') -> do
    result <- unifyKind k k'
    return (emptySubst, TType result)
  (g1@(TType k), other) -> throwError $ BadUnify g1 other
  (TInterface [], TInterface []) -> return (emptySubst, TInterface [])
  (TInterface (stmt1:stmts1), TInterface (stmt2:stmts2)) -> do
    (subs, typ) <- unify ctx (TInterface [stmt1]) (TInterface [stmt2])
    (subs2, typ2) <- unify ctx (TInterface stmts1) (TInterface stmts2)
    final_subs <- subs `composeSubst` subs2
    case (typ, typ2) of
      (TInterface x1, TInterface x2) -> return (final_subs, TInterface (x1 ++ x2))
      other -> error "should not reach"
  (t1'@(TInterface _), other) -> throwError $ BadUnify t1' t2


unifyKind :: (Monad m) => Kind -> Kind -> ContextT m Kind
unifyKind k1 k2 = case (k1, k2) of
  (KType, KType) -> return KType
  (KFun k1 k2, KFun k1' k2') -> KFun <$> unifyKind k1 k1' <*> unifyKind k2 k2'
  (k, k') -> throwError $ BadUnifyKind k k'

inferUseCount :: (Monad m) => Type -> ContextT m UseCount
inferUseCount x = case x of
  TInt -> return CAny
  TBool -> return CAny
  TChar -> return CAny
  TString -> return CAny
  TFloat -> return CAny
  TThing -> return CAny
  TUnit -> return CAny
  TVar s -> return CAny
  TFun ty ty' -> return CAny
  TPair ty ty' -> do
    uc <- inferUseCount ty
    uc' <- inferUseCount ty'
    return $ useCountPair [uc,uc']
  TSum ty ty' -> do
    res <- mapM inferUseCount [ty, ty']
    return $ maximum res
  TLabel s ty -> inferUseCount ty
  TToken s -> return CSingle
  TUsage uc ty -> inferUseCount ty >>= unifyUseCount uc
  TType ki -> return CAny
  TCall ty ty' -> return CAny
  TInterface stmts -> mapM inferUseCountStmt stmts >>= foldM unifyUseCount CAny
    where
      inferUseCountStmt :: (Monad m) => ModuleStmt -> ContextT m UseCount
      inferUseCountStmt stmt = case stmt of
        MKind s ki -> return CAny
        s@(MType _ _) -> throwError $ ConcreteTypeOrExprInInterface s
        MDecl s ty -> inferUseCount ty
        s@(MData _ _) -> throwError $ ConcreteTypeOrExprInInterface s

type Context = Map String Scheme

applySubstCtx :: (Monad m) => Substitution Type -> Context -> ContextT m Context
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
  newVars <- traverse (const newTyVar) vars
  let subst = Map.fromList (zip vars newVars)
  applySubst subst ty

inferLiteral :: (Monad m) => Lit -> ContextT m (Substitution Type, Type)
inferLiteral lit =
  return (emptySubst, case lit of
    LInt _ -> TInt
    LBool _ -> TBool
    LThing _ -> TThing
    LChar _ -> TChar
    LString _ -> TString
    LFloat _ -> TFloat
    LUnit -> TUnit)

unifyUseCount :: (Monad m) => UseCount -> UseCount -> ContextT m UseCount
unifyUseCount CSingle CMany = throwError $ BadUnifyUseCount CSingle CMany
unifyUseCount CSingle _ = return CSingle
unifyUseCount CMany CSingle = throwError $ BadUnifyUseCount CSingle CMany
unifyUseCount CMany _ = return CMany
unifyUseCount CAny other = return other

hasUseCount :: (Monad m) => UseCount -> UseCount -> ContextT m UseCount
hasUseCount CSingle CMany = throwError $ BadHasUseCount CSingle CMany
hasUseCount CSingle _ = return CSingle
hasUseCount CMany _ = return CMany
hasUseCount CAny other = return other

inferPattern :: (Monad m) => Context -> S.Set Id -> Pattern -> ContextT m (Context, Type)
inferPattern ctx copied pat = case pat of
  (PLit binder) -> do
    (_, tyBinder) <- inferLiteral binder
    return (Map.empty, TUsage CSingle tyBinder)
  (PVar binder) -> do
    tyBinder <- newTyVar
    let usage = if binder `elem` copied then CMany else CSingle
    return (Map.singleton binder (Scheme [] tyBinder), TUsage usage tyBinder)
  (PPair a b) -> do
    (ctxa, tya) <- inferPattern ctx copied a
    (ctxb, tyb) <- inferPattern ctx copied b
    counts <- mapM inferUseCount [tya, tyb]
    let usage = useCountPair counts
    case usage of
      CAny -> return (Map.union ctxa ctxb, TPair tya tyb)
      other -> return (Map.union ctxa ctxb, TUsage other $ TPair tya tyb)
  (PLabel id x) -> do
    (new_ctx, typ) <- inferPattern ctx copied x
    return (new_ctx, TLabel id typ)
  (PAnnot typ x) -> do
    (s1, typx) <- inferPattern ctx copied x
    (s2, final_typ) <- has ctx typ typx
    res <- applySubstCtx s2 s1
    return (res, final_typ)
  PWildcard -> do
    res <- newTyVar
    return (ctx, res)

infer :: (Monad m) => Context -> Exp -> ContextT m (Substitution Type, Type)
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
      inputTyp other = error "should not reach"
      outputTyp (TFun a b) = b
      outputTyp other = error "should not reach"
  EFun pattern body -> do
    (ctx_p, tyP) <- inferPattern ctx (copiedFreeRef body) pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    res <- applySubst s1 tyP
    return (s1, TFun res tyBody)
  m@(EMatch a b) -> do
    res <- newTyVar
    foo_type <- TFun res <$> newTyVar
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
            ; let (ax, ay) = tFunArgs res3
            ; let (bx, by) = tFunArgs tyB
            ; (subs_x, tyX) <- unifyJoin ax bx
            ; (subs_y, tyY) <- unifyJoin ay by
            ; res_subs <- subs_x `composeSubst` subs_y
            ; return (res_subs, TFun tyX tyY)
          })
    where
      err = throwError $ MatchWithNonFunction m
      unifyJoin a b = unify ctx a b `catchError` (\e -> return (emptySubst, TSum a b))
      tFunArgs (TFun a b) = (a,b)
      tFunArgs _ = error "should not reach"
  ELet pattern binding body -> do
    (s1, tyBinder) <- infer ctx binding
    (ctx_p, tyP) <- inferPattern ctx (copiedFreeRef body) pattern
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
    return (res, TPair subs tyB)
  ELabel id x -> do
    (s1, typ) <- infer ctx x
    return (s1, TLabel id typ)
  EAnnot typ x -> do
    (s1, typx) <- infer ctx x
    (fs, u_type) <- has ctx typ typx
    res <- fs `composeSubst` s1
    return (res, u_type)
  ERec id (EAnnot typ x) -> do
    let scheme = Scheme [] typ
    let tmpCtx = Map.insert id scheme ctx
    (subs, x) <- infer tmpCtx x
    return (subs, x)
  ERec id other -> throwError $ RecursiveWithNoTypeAnnotation other
  ETDef id t rest -> do
    let new_ctx = Map.insert id (Scheme [] t) ctx
    applySubstOnExpr (Map.singleton id t) rest >>= infer new_ctx
  EToken id h -> return (emptySubst, TToken id)
  EModule [] -> return (emptySubst, TInterface [])
  EModule (stmt:stmts) -> do
    (subst, out_typ1) <- case stmt of
          MKind s ki -> throwError $ KindDeclarationInModule stmt
          MType s ty -> (\val -> (Map.singleton s ty, TInterface [MKind s val])) <$> inferKind ty
          MDecl s ty -> do
            case Map.lookup s ctx of
              Just sc -> do
                (subs, res) <- instantiate sc >>= unify ctx ty
                return (Map.insert s res subs, TInterface [MDecl s res])
              Nothing -> return (Map.singleton s ty, TInterface [MDecl s ty])
          MData s exp' -> do
            (subst, ty) <- infer ctx exp'
            case Map.lookup s ctx of
              Just sc -> do
                (subs, res) <- instantiate sc >>= unify ctx ty
                return (Map.insert s res subst, TInterface [MDecl s res])
              Nothing -> return (Map.insert s ty subst, TInterface [MDecl s ty])
    (subst2, out_typ2) <- infer ctx (EModule stmts)
    case (out_typ1,out_typ2) of
      (TInterface m_stmt, TInterface m_stmts) -> return (Map.union subst subst2, TInterface (m_stmt ++ m_stmts))
      other -> error "bad case"

inferKind :: (Monad m) => Type -> ContextT m Kind
inferKind t = case t of
  TInt -> return KType
  TBool -> return KType
  TChar -> return KType
  TString -> return KType
  TFloat -> return KType
  TThing -> return KType
  TUnit -> return KType
  TVar s -> return KType
  TFun ty ty' -> KFun <$> inferKind ty <*> inferKind ty'
  TPair ty ty' -> do
    k1 <- inferKind ty
    k2 <- inferKind ty'
    _ <- unifyKind k1 KType
    _ <- unifyKind k2 KType
    return KType
    `catchError` (\e -> throwError $ MustOnlyHaveConcreteTypes t)
  t@(TSum ty ty') -> do
    k1 <- inferKind ty
    k2 <- inferKind ty'
    _ <- unifyKind k1 KType
    _ <- unifyKind k2 KType
    return KType
    `catchError` (\e -> throwError $ MustOnlyHaveConcreteTypes t)
  TLabel s ty -> do
    k1 <- inferKind ty
    _ <- unifyKind k1 KType
    return KType
    `catchError` (\e -> throwError $ MustOnlyHaveConcreteTypes t)
  TToken s -> return KType
  TUsage uc ty -> inferKind ty
  TType ki -> return ki
  t@(TCall ty ty') -> do
    k1 <- inferKind ty
    k2 <- inferKind ty'
    case k1 of
      KType -> throwError $ TypeCallOnConcreteType t
      KFun ki ki' -> do
        _ <- unifyKind ki k2
        return ki'
  TInterface _ -> return KType

typeInference :: (Monad m) => Context -> Exp -> ContextT m Scheme
typeInference ctx exp = do
  (s, t) <- infer ctx exp
  applySubstScheme s (generalize ctx t)

primitives :: Context
primitives = Map.empty