{-# LANGUAGE ScopedTypeVariables #-}
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

type TypeDefinitions = Map String Type

emptyTypeDefinitions :: TypeDefinitions
emptyTypeDefinitions = Map.empty

{-
applyTypeDefinitions takes a TypeDefinition map and a type, and replaces any
type var ids with the corresponding value in the definition map. 
-}
applyTypeDefinitions :: (Monad m) => TypeDefinitions -> Type -> ContextT m Type
applyTypeDefinitions defs ty = do
  case ty of
    TPair a b -> TPair <$> applyTypeDefinitions defs a <*> applyTypeDefinitions defs b
    TSum a b -> TSum <$> applyTypeDefinitions defs a <*> applyTypeDefinitions defs b
    TLabel id x -> TLabel id <$> applyTypeDefinitions defs x
    TVar var -> return $ case Map.lookup var defs of
      Nothing -> TVar var
      Just x -> x
    TFun arg res -> do
      arg' <- applyTypeDefinitions defs arg
      TFun arg' <$> applyTypeDefinitions defs res
    TInt -> return TInt
    TBool -> return TBool
    TChar -> return TChar
    TString -> return TString
    TFloat -> return TFloat
    TThing -> return TThing
    TType k -> return $ TType k
    TUnit -> return TUnit
    TCall a b -> TCall <$> applyTypeDefinitions defs a <*> applyTypeDefinitions defs b
    TToken id -> return $ TToken id
    TUsage u t -> TUsage u <$> applyTypeDefinitions defs t
    TInterface stmts -> TInterface <$> mapM (applyTypeDefinitionsOnInterfaceStmts defs) stmts

applyTypeDefinitionsOnInterfaceStmts :: (Monad m) => TypeDefinitions -> StructStmt -> ContextT m StructStmt
applyTypeDefinitionsOnInterfaceStmts defs stmt = case stmt of
  MKind s ki -> return $ MKind s ki
  MDecl s ty' -> MDecl s <$> applyTypeDefinitions defs ty'
  d@other -> throwError $ ConcreteTypeOrExprInInterface d

applyTypeDefinitionsOnPattern :: (Monad m) => TypeDefinitions -> Pattern -> ContextT m Pattern
applyTypeDefinitionsOnPattern defs (PLit l) = return $ PLit l
applyTypeDefinitionsOnPattern defs (PVar id) = return $ PVar id
applyTypeDefinitionsOnPattern defs (PPair a b) = PPair <$> applyTypeDefinitionsOnPattern defs a <*> applyTypeDefinitionsOnPattern defs b
applyTypeDefinitionsOnPattern defs (PLabel id b) = PLabel id <$> applyTypeDefinitionsOnPattern defs b
applyTypeDefinitionsOnPattern defs (PAnnot typ b) = do
  res <- applyTypeDefinitions defs typ
  PAnnot res <$> applyTypeDefinitionsOnPattern defs b
applyTypeDefinitionsOnPattern defs PWildcard = pure PWildcard

applyTypeDefinitionsOnExpr :: (Monad m) => TypeDefinitions -> Exp -> ContextT m Exp
applyTypeDefinitionsOnExpr defs t@(EVar id) = pure t
applyTypeDefinitionsOnExpr defs t@(EApp a b) = EApp <$> applyTypeDefinitionsOnExpr defs a <*> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs t@(EFun a b) = EFun <$> applyTypeDefinitionsOnPattern defs a <*> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs t@(ELet id a b) = ELet id <$> applyTypeDefinitionsOnExpr defs a <*> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs (EAnnot typ x) = EAnnot <$> applyTypeDefinitions defs typ <*> applyTypeDefinitionsOnExpr defs x
applyTypeDefinitionsOnExpr defs (ETDef id typ x) = ETDef id <$> applyTypeDefinitions defs typ <*> applyTypeDefinitionsOnExpr defs x
applyTypeDefinitionsOnExpr defs (ELit l) = pure $ ELit l
applyTypeDefinitionsOnExpr defs (ERec id r) = ERec id <$> applyTypeDefinitionsOnExpr defs r
applyTypeDefinitionsOnExpr defs (EMatch a b) = EMatch <$> applyTypeDefinitionsOnExpr defs a <*> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs (EPair a b) = EPair <$> applyTypeDefinitionsOnExpr defs a <*> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs (ELabel id b) = ELabel id <$> applyTypeDefinitionsOnExpr defs b
applyTypeDefinitionsOnExpr defs (EToken id h) = pure $ EToken id h
applyTypeDefinitionsOnExpr defs (EStruct stmts) = EStruct <$> mapM (applyTypeDefinitionsOnStructStmt defs) stmts

applyTypeDefinitionsOnStructStmt :: (Monad m) => TypeDefinitions -> StructStmt -> ContextT m StructStmt
applyTypeDefinitionsOnStructStmt defs stmt = case stmt of
  MData str exp -> MData str <$> applyTypeDefinitionsOnExpr defs exp
  other -> return other

applyTypeDefinitionsOnAbstractType :: (Monad m) => TypeDefinitions -> AbstractType -> ContextT m AbstractType
applyTypeDefinitionsOnAbstractType defs (AbstractType vars t) =
  -- The fold takes care of name shadowing
  AbstractType vars <$> applyTypeDefinitions (foldr Map.delete defs vars) t

-- | This is much more subtle than it seems. (union is left biased)
composeSubst :: (Monad m) => TypeDefinitions -> TypeDefinitions -> ContextT m TypeDefinitions
composeSubst s1 s2 = Map.union <$> mapM (applyTypeDefinitions s1) s2 <*> pure s1

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
    freeTypeVarStmts :: StructStmt -> Set Id
    freeTypeVarStmts stmt = case stmt of
      MKind s ki -> S.empty
      MType s ty' -> freeTypeVars ty'
      MDecl s ty' -> freeTypeVars ty'
      MData s exp -> S.empty


getFreeTypeVarsInAbstractType :: AbstractType -> Set Id
getFreeTypeVarsInAbstractType (AbstractType vars t) =
  Set.difference (freeTypeVars t) (Set.fromList vars)

-- | Creates a fresh unification variable and binds it to the given type
varBind :: (Monad m) => String -> Type -> ContextT m TypeDefinitions
varBind var ty
  | ty == TVar var = return emptyTypeDefinitions
  | var `Set.member` freeTypeVars ty = throwError $ OccursCheck var ty
  | otherwise = return (Map.singleton var ty)

evalTCall :: (Monad m) => AbstractTypeDefinitions -> Type -> ContextT m Type
evalTCall ctx (TCall (TVar id) b) = do
  case Map.lookup id ctx of
    Nothing -> throwError $ UnboundId id
    Just any -> do
      res <- instantiate any
      evalTCall ctx (TCall res b)
evalTCall ctx (TCall (TFun x y) b) = do
  (subs, typX) <- has ctx x b
  applyTypeDefinitions subs y
evalTCall ctx (TCall somethin b) = error $ "what is this case?: " ++ show somethin
evalTCall _ _ = error "Must Be TCall"

has :: (Monad m) => AbstractTypeDefinitions -> Type -> Type -> ContextT m (TypeDefinitions, Type)
has _ TUnit TUnit = return (emptyTypeDefinitions, TUnit)
has _ TInt TInt = return (emptyTypeDefinitions, TInt)
has _ TBool TBool = return (emptyTypeDefinitions, TBool)
has _ TChar TChar = return (emptyTypeDefinitions, TChar)
has _ TString TString = return (emptyTypeDefinitions, TString)
has _ TFloat TFloat = return (emptyTypeDefinitions, TFloat)
has _ TThing TThing = return (emptyTypeDefinitions, TThing)
has ctx a b@(TSum x y) = do
  (sx, resx) <- has ctx a x
  (sy, resy) <- has ctx a y
  final_s <- sx `composeSubst` sy
  res <- applyTypeDefinitions final_s (TSum resx resy)
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
    tryHas :: (Monad m) => Type -> Type -> ContextT m (Maybe (TypeDefinitions, Type))
    tryHas a b = (Just <$> has ctx a b) `catchError` (\e -> return Nothing)
has ctx (TFun l r) (TFun l' r') = do
  (s1, tyl) <- has ctx l l'
  tmpA <- applyTypeDefinitions s1 r
  tmpB <- applyTypeDefinitions s1 r'
  (s2, tyr) <- has ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  tfun_input <- applyTypeDefinitions final_subs l
  res <- applyTypeDefinitions final_subs $ TFun tfun_input tyr
  return (final_subs, res)
has ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- has ctx a a'
  tmpA <- applyTypeDefinitions s1 b
  tmpB <- applyTypeDefinitions s1 b'
  (s2, tyB) <- has ctx tmpA tmpB
  final_subs <- s2 `composeSubst` s1
  res <- applyTypeDefinitions final_subs (TPair tyA tyB)
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
      (,) res <$> applyTypeDefinitions res t
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
    else return (emptyTypeDefinitions, TToken id)
has ctx (TUsage u1 t1) (TUsage u2 t2) = do
  (defs, typ) <- has ctx t1 t2
  usage <- hasUseCount u1 u2
  return (defs, TUsage usage typ)
has ctx (TUsage u1 t1) t2 = has ctx (TUsage u1 t1) (TUsage CAny t2)
has ctx t1 (TUsage u2 t2)  = has ctx (TUsage CAny t1) (TUsage u2 t2)
{- Fix below to properly check subtypes -}
has ctx t1@(TInterface stmts) (TInterface stmts2) = return (emptyTypeDefinitions, t1)
has _ t1 t2 = throwError $ BadHas t1 t2


unify :: (Monad m) => AbstractTypeDefinitions -> Type -> Type -> ContextT m (TypeDefinitions, Type)
unify ctx t1 t2 = case (t1, t2) of
  (TUnit, TUnit) -> return (emptyTypeDefinitions, TUnit)
  (TInt, TInt) -> return (emptyTypeDefinitions, TInt)
  (TBool, TBool) -> return (emptyTypeDefinitions, TBool)
  (TChar, TChar) -> return (emptyTypeDefinitions, TChar)
  (TString, TString) -> return (emptyTypeDefinitions, TString)
  (TFloat, TFloat) -> return (emptyTypeDefinitions, TFloat)
  (TThing, TThing) -> return (emptyTypeDefinitions, TThing)
  (TUnit, t2') -> throwError $ BadUnify TUnit t2'
  (TInt, t2') -> throwError $ BadUnify TInt t2'
  (TBool, t2') -> throwError $ BadUnify TBool t2'
  (TChar, t2') -> throwError $ BadUnify TChar t2'
  (TString, t2') -> throwError $ BadUnify TString t2'
  (TFloat, t2') -> throwError $ BadUnify TFloat t2'
  (TThing, t2') -> throwError $ BadUnify TThing t2'
  (TFun l r, TFun l' r') -> do
    (s1, tyl) <- unify ctx l l'
    tmpA <- applyTypeDefinitions s1 r
    tmpB <- applyTypeDefinitions s1 r'
    (s2, tyr) <- unify ctx tmpA tmpB
    final_subs <- s2 `composeSubst` s1
    tfun_input <- applyTypeDefinitions final_subs l
    res <- applyTypeDefinitions final_subs $ TFun tfun_input tyr
    return (final_subs, res)
  (t1'@(TFun _ _), t2') -> throwError $ BadUnify t1' t2'
  (TPair a b, TPair a' b') -> do
    (s1, tyA) <- unify ctx a a'
    tmpA <- applyTypeDefinitions s1 b
    tmpB <- applyTypeDefinitions s1 b'
    (s2, tyB) <- unify ctx tmpA tmpB
    final_subs <- s2 `composeSubst` s1
    res <- applyTypeDefinitions final_subs (TPair tyA tyB)
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
        (,) res <$> applyTypeDefinitions res t
      Just any -> do
        resT <- instantiate any
        unify ctx resT t
  (TCall _ _, t) -> do
    res <- evalTCall ctx t1
    unify ctx res t
  (TSum x y, TSum x' y') ->
    trySumUnify x x' y y' `catchError` (\e -> trySumUnify x y' y x')
    where
      trySumUnify :: (Monad m) => Type -> Type -> Type -> Type -> ContextT m (TypeDefinitions, Type)
      trySumUnify x x' y y' = do
        (sub_x, typX) <- unify ctx x x'
        (sub_y, typY) <- unify ctx y y'
        final_subs <- sub_x `composeSubst` sub_y
        res <- TSum <$> applyTypeDefinitions final_subs typX <*> applyTypeDefinitions final_subs typY
        return (final_subs, res)
  (t1'@(TSum _ _), t2') -> throwError $ BadUnify t1' t2'
  (TToken id, TToken id2) ->
    if id /= id2
      then throwError $ BadUnify t1 t2
      else return (emptyTypeDefinitions, TToken id)
  (t1'@(TToken _), t2') -> throwError $ BadUnify t1' t2
  (TUsage u1 t1, TUsage u2 t2) -> do
    (defs, typ) <- unify ctx t1 t2
    usage <- unifyUseCount u1 u2
    return (defs, TUsage usage typ)
  (TUsage u1 t1, t2) -> do
    usage_t2 <- inferUseCount t2
    unify ctx (TUsage u1 t1) (TUsage usage_t2 t2)
  (t1, TUsage u2 t2) -> unify ctx (TUsage CAny t1) (TUsage u2 t2)
  (TType k, TType k') -> do
    result <- unifyKind k k'
    return (emptyTypeDefinitions, TType result)
  (g1@(TType k), other) -> throwError $ BadUnify g1 other
  (TInterface [], TInterface []) -> return (emptyTypeDefinitions, TInterface [])
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
      inferUseCountStmt :: (Monad m) => StructStmt -> ContextT m UseCount
      inferUseCountStmt stmt = case stmt of
        MKind s ki -> return CAny
        s@(MType _ _) -> throwError $ ConcreteTypeOrExprInInterface s
        MDecl s ty -> inferUseCount ty
        s@(MData _ _) -> throwError $ ConcreteTypeOrExprInInterface s

type AbstractTypeDefinitions = Map String AbstractType

applyTypeDefinitionsOnAbstractTypeDefinitions :: (Monad m) => TypeDefinitions -> AbstractTypeDefinitions -> ContextT m AbstractTypeDefinitions
applyTypeDefinitionsOnAbstractTypeDefinitions defs = mapM (applyTypeDefinitionsOnAbstractType defs)

getFreeTypeVarsInAbstractTypeDefinitions :: AbstractTypeDefinitions -> Set String
getFreeTypeVarsInAbstractTypeDefinitions aDefs = foldMap getFreeTypeVarsInAbstractType (Map.elems aDefs)

generalize :: AbstractTypeDefinitions -> Type -> AbstractType
generalize aDefs t = AbstractType vars t
  where
    vars = Set.toList (Set.difference (freeTypeVars t) (getFreeTypeVarsInAbstractTypeDefinitions aDefs))

instantiate :: (Monad m) => AbstractType -> ContextT m Type
instantiate (AbstractType vars ty) = do
  newVars <- traverse (const newTyVar) vars
  let defs = Map.fromList (zip vars newVars)
  applyTypeDefinitions defs ty

inferLiteral :: (Monad m) => Lit -> ContextT m (TypeDefinitions, Type)
inferLiteral lit =
  return (emptyTypeDefinitions, case lit of
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

inferPattern :: (Monad m) => AbstractTypeDefinitions -> S.Set Id -> Pattern -> ContextT m (AbstractTypeDefinitions, Type)
inferPattern ctx copied pat = case pat of
  (PLit binder) -> do
    (_, tyBinder) <- inferLiteral binder
    return (Map.empty, TUsage CSingle tyBinder)
  (PVar binder) -> do
    tyBinder <- newTyVar
    let usage = if binder `elem` copied then CMany else CSingle
    return (Map.singleton binder (AbstractType [] tyBinder), TUsage usage tyBinder)
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
    res <- applyTypeDefinitionsOnAbstractTypeDefinitions s2 s1
    return (res, final_typ)
  PWildcard -> do
    res <- newTyVar
    return (ctx, res)

infer :: (Monad m) => AbstractTypeDefinitions -> Exp -> ContextT m (TypeDefinitions, Type)
infer ctx exp = case exp of
  EVar var -> case Map.lookup var ctx of
    Nothing -> throwError $ UnboundId var
    Just scheme -> do
      ty <- instantiate scheme
      return (emptyTypeDefinitions, ty)
  ELit lit ->
    inferLiteral lit
  EApp fun arg -> do
    (s1, tyFun) <- infer ctx fun
    (s2, tyArg) <- applyTypeDefinitionsOnAbstractTypeDefinitions s1 ctx >>= infer <*> pure arg
    tmp <- applyTypeDefinitions s2 (inputTyp tyFun)
    (s3, input_typ) <- has ctx tmp tyArg
    final_defs <- s3 `composeSubst` s2 >>= composeSubst s1
    res <- applyTypeDefinitions final_defs tyFun
    return (final_defs, outputTyp res)
    where
      inputTyp (TFun a b) = a
      inputTyp other = error "should not reach"
      outputTyp (TFun a b) = b
      outputTyp other = error "should not reach"
  EFun pattern body -> do
    (ctx_p, tyP) <- inferPattern ctx (copiedFreeRef body) pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    res <- applyTypeDefinitions s1 tyP
    return (s1, TFun res tyBody)
  m@(EMatch a b) -> do
    res <- newTyVar
    foo_type <- TFun res <$> newTyVar
    (s1, tyA) <- infer ctx a
    if not $ isTFun tyA
      then err
    else do
      res <- applyTypeDefinitionsOnAbstractTypeDefinitions s1 ctx
      (s2, tyB) <- infer res b
      if not $ isTFun tyB
        then err
      else do
        res2 <- applyTypeDefinitions s2 tyA
        (final_subs, final_ty) <- unify ctx res2 tyB
        case final_ty of
          TFun _ _ -> return (final_subs, final_ty)
          other -> err
        `catchError` (\e -> do {
              res3 <- applyTypeDefinitions s2 tyA
            ; let (ax, ay) = tFunArgs res3
            ; let (bx, by) = tFunArgs tyB
            ; (subs_x, tyX) <- unifyJoin ax bx
            ; (subs_y, tyY) <- unifyJoin ay by
            ; res_subs <- subs_x `composeSubst` subs_y
            ; return (res_subs, TFun tyX tyY)
          })
    where
      err = throwError $ MatchWithNonFunction m
      unifyJoin a b = unify ctx a b `catchError` (\e -> return (emptyTypeDefinitions, TSum a b))
      tFunArgs (TFun a b) = (a,b)
      tFunArgs _ = error "should not reach"
  ELet pattern binding body -> do
    (s1, tyBinder) <- infer ctx binding
    (ctx_p, tyP) <- inferPattern ctx (copiedFreeRef body) pattern
    let newCtx = Map.union ctx_p ctx
    res <- applyTypeDefinitions s1 tyBinder
    (s2, typB) <- unify newCtx tyP res
    tmpA <- s2 `composeSubst` s1
    patCtx <- applyTypeDefinitionsOnAbstractTypeDefinitions tmpA ctx_p
    finalCtx <- applyTypeDefinitionsOnAbstractTypeDefinitions tmpA $ Map.union patCtx ctx
    (s3, tyBody) <- infer finalCtx body
    (,) <$> s3 `composeSubst` tmpA <*> pure tyBody
  EPair a b -> do
    (s1, tyA) <- infer ctx a
    tmp <- applyTypeDefinitionsOnAbstractTypeDefinitions s1 ctx
    (s2, tyB) <- infer tmp b
    res <- s2 `composeSubst` s1
    subs <- applyTypeDefinitions s2 tyA
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
    let scheme = AbstractType [] typ
    let tmpCtx = Map.insert id scheme ctx
    (subs, x) <- infer tmpCtx x
    return (subs, x)
  ERec id other -> throwError $ RecursiveWithNoTypeAnnotation other
  ETDef id t rest -> do
    let new_ctx = Map.insert id (AbstractType [] t) ctx
    applyTypeDefinitionsOnExpr (Map.singleton id t) rest >>= infer new_ctx
  EToken id h -> return (emptyTypeDefinitions, TToken id)
  EStruct [] -> return (emptyTypeDefinitions, TInterface [])
  EStruct (stmt:stmts) -> do
    (defs, out_typ1) <- case stmt of
          MKind s ki -> throwError $ KindDeclarationInStruct stmt
          MType s ty -> (\val -> (Map.singleton s ty, TInterface [MKind s val])) <$> inferKind ty
          MDecl s ty -> do
            case Map.lookup s ctx of
              Just sc -> do
                (subs, res) <- instantiate sc >>= unify ctx ty
                return (Map.insert s res subs, TInterface [MDecl s res])
              Nothing -> return (Map.singleton s ty, TInterface [MDecl s ty])
          MData s exp' -> do
            (defs, ty) <- infer ctx exp'
            case Map.lookup s ctx of
              Just sc -> do
                (subs, res) <- instantiate sc >>= unify ctx ty
                return (Map.insert s res defs, TInterface [MDecl s res])
              Nothing -> return (Map.insert s ty defs, TInterface [MDecl s ty])
    (defs2, out_typ2) <- infer ctx (EStruct stmts)
    case (out_typ1,out_typ2) of
      (TInterface m_stmt, TInterface m_stmts) -> return (Map.union defs defs2, TInterface (m_stmt ++ m_stmts))
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

typeInference :: (Monad m) => AbstractTypeDefinitions -> Exp -> ContextT m AbstractType
typeInference aDefs exp = do
  (tDefs, typ) <- infer aDefs exp
  generalize aDefs <$> applyTypeDefinitions tDefs typ