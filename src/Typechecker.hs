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

emptyTEnv :: TEnv
emptyTEnv = Map.empty

{-
applyTEnv takes a TypeDefinition map and a type, and replaces any
type var ids with the corresponding value in the definition map. 
-}
applyTEnv :: (Monad m) => TEnv -> Type -> ContextT m Type
applyTEnv defs ty = do
  case ty of
    TPair a b -> TPair <$> applyTEnv defs a <*> applyTEnv defs b
    TSum a b -> TSum <$> applyTEnv defs a <*> applyTEnv defs b
    TLabel id x -> TLabel id <$> applyTEnv defs x
    TVar var -> return $ case Map.lookup var defs of
      Nothing -> TVar var
      Just x -> x
    TFun arg res -> do
      arg' <- applyTEnv defs arg
      TFun arg' <$> applyTEnv defs res
    TInt -> return TInt
    TBool -> return TBool
    TChar -> return TChar
    TString -> return TString
    TFloat -> return TFloat
    TThing -> return TThing
    TType k -> return $ TType k
    TUnit -> return TUnit
    TCall a b -> TCall <$> applyTEnv defs a <*> applyTEnv defs b
    TToken id -> return $ TToken id
    TUsage u t -> TUsage u <$> applyTEnv defs t
    TInterface stmts -> TInterface <$> mapM (applyTEnvOnInterfaceStmts defs) stmts

applyTEnvOnInterfaceStmts :: (Monad m) => TEnv -> StructStmt -> ContextT m StructStmt
applyTEnvOnInterfaceStmts defs stmt = case stmt of
  MDecl s ty' -> MDecl s <$> applyTEnv defs ty'
  d@other -> throwError $ ConcreteTypeOrExprInInterface d

applyTEnvOnPattern :: (Monad m) => TEnv -> Pattern -> ContextT m Pattern
applyTEnvOnPattern defs (PLit l) = return $ PLit l
applyTEnvOnPattern defs (PVar id) = return $ PVar id
applyTEnvOnPattern defs (PPair a b) = PPair <$> applyTEnvOnPattern defs a <*> applyTEnvOnPattern defs b
applyTEnvOnPattern defs (PLabel id b) = PLabel id <$> applyTEnvOnPattern defs b
applyTEnvOnPattern defs (PAnnot typ b) = do
  res <- applyTEnv defs typ
  PAnnot res <$> applyTEnvOnPattern defs b
applyTEnvOnPattern defs PWildcard = pure PWildcard

applyTEnvOnExpr :: (Monad m) => TEnv -> Exp -> ContextT m Exp
applyTEnvOnExpr defs t@(EVar id) = pure t
applyTEnvOnExpr defs t@(EApp a b) = EApp <$> applyTEnvOnExpr defs a <*> applyTEnvOnExpr defs b
applyTEnvOnExpr defs t@(EFun a b) = EFun <$> applyTEnvOnPattern defs a <*> applyTEnvOnExpr defs b
applyTEnvOnExpr defs t@(ELet id a b) = ELet id <$> applyTEnvOnExpr defs a <*> applyTEnvOnExpr defs b
applyTEnvOnExpr defs (EAnnot typ x) = EAnnot <$> applyTEnv defs typ <*> applyTEnvOnExpr defs x
applyTEnvOnExpr defs (ETDef id typ x) = ETDef id <$> applyTEnv defs typ <*> applyTEnvOnExpr defs x
applyTEnvOnExpr defs (ELit l) = pure $ ELit l
applyTEnvOnExpr defs (ERec id r) = ERec id <$> applyTEnvOnExpr defs r
applyTEnvOnExpr defs (EMatch a b) = EMatch <$> applyTEnvOnExpr defs a <*> applyTEnvOnExpr defs b
applyTEnvOnExpr defs (EPair a b) = EPair <$> applyTEnvOnExpr defs a <*> applyTEnvOnExpr defs b
applyTEnvOnExpr defs (ELabel id b) = ELabel id <$> applyTEnvOnExpr defs b
applyTEnvOnExpr defs (EToken id h) = pure $ EToken id h
applyTEnvOnExpr defs (EStruct stmts) = EStruct <$> mapM (applyTEnvOnStructStmt defs) stmts

applyTEnvOnStructStmt :: (Monad m) => TEnv -> StructStmt -> ContextT m StructStmt
applyTEnvOnStructStmt defs stmt = case stmt of
  MExpr str exp -> MExpr str <$> applyTEnvOnExpr defs exp
  other -> return other

applyTEnvOnAbstractType :: (Monad m) => TEnv -> AbstractType -> ContextT m AbstractType
applyTEnvOnAbstractType defs (AbstractType vars t) =
  -- The fold takes care of name shadowing
  AbstractType vars <$> applyTEnv (foldr Map.delete defs vars) t

-- | This is much more subtle than it seems. (union is left biased)
updateTEnv :: (Monad m) => TEnv -> TEnv -> ContextT m TEnv
updateTEnv s1 s2 = Map.union <$> mapM (applyTEnv s1) s2 <*> pure s1

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
      MType s ty' -> freeTypeVars ty'
      MDecl s ty' -> freeTypeVars ty'
      MExpr s exp -> S.empty


getFreeTypeVarsInAbstractType :: AbstractType -> Set Id
getFreeTypeVarsInAbstractType (AbstractType vars t) =
  Set.difference (freeTypeVars t) (Set.fromList vars)

-- | Creates a fresh unification variable and binds it to the given type
varBind :: (Monad m) => String -> Type -> ContextT m TEnv
varBind var ty
  | ty == TVar var = return Map.empty
  | var `Set.member` freeTypeVars ty = throwError $ OccursCheck var ty
  | otherwise = return (Map.singleton var ty)

evalTCall :: (Monad m) => AbstractTEnv -> Type -> ContextT m Type
evalTCall ctx (TCall (TVar id) b) = do
  case Map.lookup id ctx of
    Nothing -> throwError $ UnboundId id
    Just any -> do
      res <- instantiate any
      evalTCall ctx (TCall res b)
evalTCall ctx (TCall (TFun x y) b) = do
  (subs, typX) <- has ctx x b
  applyTEnv subs y
evalTCall ctx (TCall somethin b) = error $ "what is this case?: " ++ show somethin
evalTCall _ _ = error "Must Be TCall"

has :: (Monad m) => AbstractTEnv -> Type -> Type -> ContextT m (TEnv, Type)
has _ TUnit TUnit = return (Map.empty, TUnit)
has _ TInt TInt = return (Map.empty, TInt)
has _ TBool TBool = return (Map.empty, TBool)
has _ TChar TChar = return (Map.empty, TChar)
has _ TString TString = return (Map.empty, TString)
has _ TFloat TFloat = return (Map.empty, TFloat)
has _ TThing TThing = return (Map.empty, TThing)
has ctx a b@(TSum x y) = do
  (sx, resx) <- has ctx a x
  (sy, resy) <- has ctx a y
  final_s <- sx `updateTEnv` sy
  res <- applyTEnv final_s (TSum resx resy)
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
    tryHas :: (Monad m) => Type -> Type -> ContextT m (Maybe (TEnv, Type))
    tryHas a b = (Just <$> has ctx a b) `catchError` (\e -> return Nothing)
has ctx (TFun l r) (TFun l' r') = do
  (s1, tyl) <- has ctx l l'
  tmpA <- applyTEnv s1 r
  tmpB <- applyTEnv s1 r'
  (s2, tyr) <- has ctx tmpA tmpB
  final_subs <- s2 `updateTEnv` s1
  tfun_input <- applyTEnv final_subs l
  res <- applyTEnv final_subs $ TFun tfun_input tyr
  return (final_subs, res)
has ctx (TPair a b) (TPair a' b') = do
  (s1, tyA) <- has ctx a a'
  tmpA <- applyTEnv s1 b
  tmpB <- applyTEnv s1 b'
  (s2, tyB) <- has ctx tmpA tmpB
  final_subs <- s2 `updateTEnv` s1
  res <- applyTEnv final_subs (TPair tyA tyB)
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
      (,) res <$> applyTEnv res t
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
    else return (Map.empty, TToken id)
has ctx (TUsage u1 t1) (TUsage u2 t2) = do
  (defs, typ) <- has ctx t1 t2
  usage <- hasUseCount u1 u2
  return (defs, TUsage usage typ)
has ctx (TUsage u1 t1) t2 = has ctx (TUsage u1 t1) (TUsage CAny t2)
has ctx t1 (TUsage u2 t2)  = has ctx (TUsage CAny t1) (TUsage u2 t2)
{- Fix below to properly check subtypes -}
has ctx t1@(TInterface stmts1) t2@(TInterface stmts2) = do
  let (types1, decl1, _) = splitStructStmts stmts1
  decl1 <- mapM (applyTEnv types1) decl1
  let (types2, decl2, _) = splitStructStmts stmts2
  decl2 <- mapM (applyTEnv types2) decl2
  let key_diff = Map.keysSet decl1 `Set.difference` Map.keysSet decl2

  if not $ Set.null key_diff
    then throwError $ BadHas t1 t2
  else do
    result <- hasMatchingTypes ctx decl1 decl2
    new_aDefs <- foldrM (\(tenv, typ) accum -> accum `updateTEnv` tenv) Map.empty result
    let interface_as_map = Map.map snd result
    let interface_result = Map.foldrWithKey (\k v accum -> MDecl k v : accum ) [] interface_as_map :: [StructStmt]
    return (new_aDefs, TInterface interface_result)
  where
    createTypePairsMap :: TEnv -> TEnv -> Map.Map Id (Type, Type)
    createTypePairsMap map1 map2 = Map.fromList [(id, (map1 Map.! id, map2 Map.! id)) | id <- Set.toList (Map.keysSet map1 `Set.intersection` Map.keysSet map2)]
    
    hasMatchingTypes :: (Monad m) => AbstractTEnv -> TEnv -> TEnv -> ContextT m (Map Id (TEnv, Type))
    hasMatchingTypes aDefs map1 map2 =
      mapM (\(a,b) -> has aDefs a b) (createTypePairsMap map1 map2) :: (Monad m) => ContextT m (Map Id (TEnv, Type))
has _ t1 t2 = throwError $ BadHas t1 t2


unify :: (Monad m) => AbstractTEnv -> Type -> Type -> ContextT m (TEnv, Type)
unify ctx t1 t2 = case (t1, t2) of
  (TUnit, TUnit) -> return (Map.empty, TUnit)
  (TInt, TInt) -> return (Map.empty, TInt)
  (TBool, TBool) -> return (Map.empty, TBool)
  (TChar, TChar) -> return (Map.empty, TChar)
  (TString, TString) -> return (Map.empty, TString)
  (TFloat, TFloat) -> return (Map.empty, TFloat)
  (TThing, TThing) -> return (Map.empty, TThing)
  (TUnit, t2') -> throwError $ BadUnify TUnit t2'
  (TInt, t2') -> throwError $ BadUnify TInt t2'
  (TBool, t2') -> throwError $ BadUnify TBool t2'
  (TChar, t2') -> throwError $ BadUnify TChar t2'
  (TString, t2') -> throwError $ BadUnify TString t2'
  (TFloat, t2') -> throwError $ BadUnify TFloat t2'
  (TThing, t2') -> throwError $ BadUnify TThing t2'
  (TFun l r, TFun l' r') -> do
    (s1, tyl) <- unify ctx l l'
    tmpA <- applyTEnv s1 r
    tmpB <- applyTEnv s1 r'
    (s2, tyr) <- unify ctx tmpA tmpB
    final_subs <- s2 `updateTEnv` s1
    tfun_input <- applyTEnv final_subs l
    res <- applyTEnv final_subs $ TFun tfun_input tyr
    return (final_subs, res)
  (t1'@(TFun _ _), t2') -> throwError $ BadUnify t1' t2'
  (TPair a b, TPair a' b') -> do
    (s1, tyA) <- unify ctx a a'
    tmpA <- applyTEnv s1 b
    tmpB <- applyTEnv s1 b'
    (s2, tyB) <- unify ctx tmpA tmpB
    final_subs <- s2 `updateTEnv` s1
    res <- applyTEnv final_subs (TPair tyA tyB)
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
        (,) res <$> applyTEnv res t
      Just any -> do
        resT <- instantiate any
        unify ctx resT t
  (TCall _ _, t) -> do
    res <- evalTCall ctx t1
    unify ctx res t
  (TSum x y, TSum x' y') ->
    trySumUnify x x' y y' `catchError` (\e -> trySumUnify x y' y x')
    where
      trySumUnify :: (Monad m) => Type -> Type -> Type -> Type -> ContextT m (TEnv, Type)
      trySumUnify x x' y y' = do
        (sub_x, typX) <- unify ctx x x'
        (sub_y, typY) <- unify ctx y y'
        final_subs <- sub_x `updateTEnv` sub_y
        res <- TSum <$> applyTEnv final_subs typX <*> applyTEnv final_subs typY
        return (final_subs, res)
  (t1'@(TSum _ _), t2') -> throwError $ BadUnify t1' t2'
  (TToken id, TToken id2) ->
    if id /= id2
      then throwError $ BadUnify t1 t2
      else return (Map.empty, TToken id)
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
    return (Map.empty, TType result)
  (g1@(TType k), other) -> throwError $ BadUnify g1 other
  (t1@(TInterface stmts1), t2@(TInterface stmts2)) -> do
    let (types1, decl1, _) = splitStructStmts stmts1
    decl1 <- mapM (applyTEnv types1) decl1
    let (types2, decl2, _) = splitStructStmts stmts2
    decl2 <- mapM (applyTEnv types2) decl2
    if Map.keysSet decl1 /= Map.keysSet decl2
      then throwError $ BadUnify t1 t2
    else do
      result <- unifyMatchingTypes ctx decl1 decl2
      new_aDefs <- foldrM (\(tenv, typ) accum -> accum `updateTEnv` tenv) Map.empty result
      let interface_as_map = Map.map snd result
      let interface_result = Map.foldrWithKey (\k v accum -> MDecl k v : accum ) [] interface_as_map :: [StructStmt]
      return (new_aDefs, TInterface interface_result)
    where
      createTypePairsMap :: TEnv -> TEnv -> Map.Map Id (Type, Type)
      createTypePairsMap map1 map2 = Map.fromList [(id, (map1 Map.! id, map2 Map.! id)) | id <- Set.toList (Map.keysSet map1 `Set.intersection` Map.keysSet map2)]
      
      unifyMatchingTypes :: (Monad m) => AbstractTEnv -> TEnv -> TEnv -> ContextT m (Map Id (TEnv, Type))
      unifyMatchingTypes aDefs map1 map2 =
        mapM (\(a,b) -> unify aDefs a b) (createTypePairsMap map1 map2) :: (Monad m) => ContextT m (Map Id (TEnv, Type))
        

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
        s@(MType _ _) -> throwError $ ConcreteTypeOrExprInInterface s
        MDecl s ty -> inferUseCount ty
        s@(MExpr _ _) -> throwError $ ConcreteTypeOrExprInInterface s

applyTEnvOnAbstractTEnv :: (Monad m) => TEnv -> AbstractTEnv -> ContextT m AbstractTEnv
applyTEnvOnAbstractTEnv defs = mapM (applyTEnvOnAbstractType defs)

getFreeTypeVarsInAbstractTEnv :: AbstractTEnv -> Set String
getFreeTypeVarsInAbstractTEnv aDefs = foldMap getFreeTypeVarsInAbstractType (Map.elems aDefs)

generalize :: AbstractTEnv -> Type -> AbstractType
generalize aDefs t = AbstractType vars t
  where
    vars = Set.toList (Set.difference (freeTypeVars t) (getFreeTypeVarsInAbstractTEnv aDefs))

generalizeTEnv :: AbstractTEnv -> TEnv -> AbstractTEnv
generalizeTEnv aDefs = Map.map (generalize aDefs)

abstractifyTEnv :: TEnv -> AbstractTEnv
abstractifyTEnv = Map.map (AbstractType [])

instantiate :: (Monad m) => AbstractType -> ContextT m Type
instantiate (AbstractType vars ty) = do
  newVars <- traverse (const newTyVar) vars
  let defs = Map.fromList (zip vars newVars)
  applyTEnv defs ty

inferLiteral :: (Monad m) => Lit -> ContextT m (TEnv, Type)
inferLiteral lit =
  return (Map.empty, case lit of
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

inferPattern :: (Monad m) => AbstractTEnv -> S.Set Id -> Pattern -> ContextT m (TEnv, Type)
inferPattern ctx copied pat = case pat of
  (PLit binder) -> do
    (_, tyBinder) <- inferLiteral binder
    return (Map.empty, TUsage CSingle tyBinder)
  (PVar binder) -> do
    tyBinder <- newTyVar
    let usage = if binder `elem` copied then CMany else CSingle
    return (Map.singleton binder tyBinder, TUsage usage tyBinder)
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
    res <- s2 `updateTEnv` s1
    return (res, final_typ)
  PWildcard -> do
    res <- newTyVar
    return (Map.empty, res)

infer :: (Monad m) => AbstractTEnv -> Exp -> ContextT m (TEnv, Type)
infer aDefs exp = case exp of
  EVar var -> case Map.lookup var aDefs of
    Nothing -> throwError $ UnboundId var
    Just scheme -> do
      ty <- instantiate scheme
      return (Map.empty, ty)
  ELit lit ->
    inferLiteral lit
  EApp fun arg -> do
    (s1, tyFun) <- infer aDefs fun
    (s2, tyArg) <- applyTEnvOnAbstractTEnv s1 aDefs >>= infer <*> pure arg
    tmp <- applyTEnv s2 (inputTyp tyFun)
    (s3, input_typ) <- has aDefs tmp tyArg
    final_defs <- s3 `updateTEnv` s2 >>= updateTEnv s1
    res <- applyTEnv final_defs tyFun
    return (final_defs, outputTyp res)
    where
      inputTyp (TFun a b) = a
      inputTyp other = error "should not reach"
      outputTyp (TFun a b) = b
      outputTyp other = error "should not reach"
  EFun pattern body -> do
    -- figure out pattern type
    (pattern_env, pattern_typ) <- inferPattern aDefs (copiedFreeRef body) pattern
    -- with the context of the pattern types, figure out body types
    let general_ctx_p = Map.union (abstractifyTEnv pattern_env) aDefs
    (body_ctx, body_typ) <- infer general_ctx_p body
    final_pattern_typ <- applyTEnv body_ctx pattern_typ
    return (body_ctx, TFun final_pattern_typ body_typ)
  m@(EMatch a b) -> do
    res <- newTyVar
    foo_type <- TFun res <$> newTyVar
    (s1, tyA) <- infer aDefs a
    if not $ isTFun tyA
      then err
    else do
      res <- applyTEnvOnAbstractTEnv s1 aDefs
      (s2, tyB) <- infer res b
      if not $ isTFun tyB
        then err
      else do
        res2 <- applyTEnv s2 tyA
        (final_subs, final_ty) <- unify aDefs res2 tyB
        case final_ty of
          TFun _ _ -> return (final_subs, final_ty)
          other -> err
        `catchError` (\e -> do {
              res3 <- applyTEnv s2 tyA
            ; let (ax, ay) = tFunArgs res3
            ; let (bx, by) = tFunArgs tyB
            ; (subs_x, tyX) <- unifyJoin ax bx
            ; (subs_y, tyY) <- unifyJoin ay by
            ; res_subs <- subs_x `updateTEnv` subs_y
            ; return (res_subs, TFun tyX tyY)
          })
    where
      err = throwError $ MatchWithNonFunction m
      unifyJoin a b = unify aDefs a b `catchError` (\e -> return (Map.empty, TSum a b))
      tFunArgs (TFun a b) = (a,b)
      tFunArgs _ = error "should not reach"
  ELet pattern bind_exp body -> do
    (bind_exp_ctx, bind_exp_typ) <- infer aDefs bind_exp
    (pattern_ctx, pattern_typ) <- inferPattern aDefs (copiedFreeRef body) pattern
    (bound_ctx, bound_typ) <- unify aDefs pattern_typ bind_exp_typ
    final_ctx <- mapM (applyTEnv bound_ctx) pattern_ctx
    let abstract_final_ctx =  generalizeTEnv aDefs final_ctx `Map.union` aDefs
    (body_ctx, body_typ) <- infer abstract_final_ctx body
    return (body_ctx, body_typ)
  EPair a b -> do
    (s1, tyA) <- infer aDefs a
    tmp <- applyTEnvOnAbstractTEnv s1 aDefs
    (s2, tyB) <- infer tmp b
    res <- s2 `updateTEnv` s1
    subs <- applyTEnv s2 tyA
    return (res, TPair subs tyB)
  ELabel id x -> do
    (s1, typ) <- infer aDefs x
    return (s1, TLabel id typ)
  EAnnot typ x -> do
    (s1, typx) <- infer aDefs x
    (fs, u_type) <- has aDefs typ typx
    res <- fs `updateTEnv` s1
    return (res, u_type)
  ERec id (EAnnot typ x) -> do
    let scheme = AbstractType [] typ
    let tmpCtx = Map.insert id scheme aDefs
    (subs, x) <- infer tmpCtx x
    return (subs, x)
  ERec id other -> throwError $ RecursiveWithNoTypeAnnotation other
  ETDef id t rest -> do
    let new_ctx = Map.insert id (AbstractType [] t) aDefs
    applyTEnvOnExpr (Map.singleton id t) rest >>= infer new_ctx
  EToken id h -> return (Map.empty, TToken id)
  e@(EStruct stmts) -> do
    let (types, decl, expr) = splitStructStmts stmts
    let decl_not_expr = Map.keysSet decl `Set.difference` Map.keysSet expr
    if not $ Set.null decl_not_expr
      then throwError $ UndefinedDeclarations e (Set.toList decl_not_expr)
    else do
      decl <- mapM (applyTEnv types) decl
      inferred_results <- mapM (infer aDefs) expr
      aDefs <- foldrM (\ (tenv, _) accum -> accum `updateTEnv` tenv) Map.empty inferred_results
      let interface_as_map = Map.map snd inferred_results
      let interface_result = Map.foldrWithKey (\k v accum -> MDecl k v : accum ) [] interface_as_map :: [StructStmt]
      return (aDefs, TInterface interface_result)

splitStructStmts :: [StructStmt] -> (TEnv, TEnv, Env)
splitStructStmts = foldr accumulate (Map.empty, Map.empty, Map.empty)
  where
    accumulate (MType id typ) (map1, map2, map3) = (Map.insert id typ map1, map2, map3)
    accumulate (MDecl id typ) (map1, map2, map3) = (map1, Map.insert id typ map2, map3)
    accumulate (MExpr id exp) (map1, map2, map3) = (map1, map2, Map.insert id exp map3)


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

typeInference :: (Monad m) => AbstractTEnv -> Exp -> ContextT m AbstractType
typeInference aDefs exp = do
  (tDefs, typ) <- infer aDefs exp
  generalize aDefs <$> applyTEnv tDefs typ

primitives :: AbstractTEnv
primitives = Map.empty