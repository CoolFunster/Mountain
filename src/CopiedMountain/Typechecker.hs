{-# language OverloadedStrings #-}
module CopiedMountain.Typechecker where

import CopiedMountain.PrettyPrinter

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

type Substitution = Map Text Type

emptySubst :: Substitution
emptySubst = Map.empty

applySubst :: Substitution -> Type -> Type
applySubst subst ty = case ty of
  TPair a b -> TPair (applySubst subst a) (applySubst subst b)
  TSum a b -> TSum (applySubst subst a) (applySubst subst b)
  TLabel id x -> TLabel id (applySubst subst x)
  TVar var ->
    fromMaybe (TVar var) (Map.lookup var subst)
  TFun arg res ->
    TFun (applySubst subst arg) (applySubst subst res)
  TInt -> TInt
  TBool -> TBool
  TChar -> TChar
  TString -> TString
  TFloat -> TFloat
  TThing -> TThing

applySubstScheme :: Substitution -> Scheme -> Scheme
applySubstScheme subst (Scheme vars t) =
  -- The fold takes care of name shadowing
  Scheme vars (applySubst (foldr Map.delete subst vars) t)

-- | This is much more subtle than it seems. (union is left biased)
--
composeSubst :: Substitution -> Substitution -> Substitution
composeSubst s1 s2 = Map.union (Map.map (applySubst s1) s2) s1

type TI a = ExceptT Text (State Int) a

runTI :: TI a -> (Either Text a, Int)
runTI ti = runState (runExceptT ti) 0

-- | Creates a fresh type variable
newTyVar :: TI Type
newTyVar = do
  s <- get
  put (s + 1)
  return (TVar ("u" <> showT s))

freeTypeVars :: Type -> Set Text
freeTypeVars ty = case ty of
 TVar var ->
   Set.singleton var
 TFun t1 t2 ->
   Set.union (freeTypeVars t1) (freeTypeVars t2)
 _ ->
   Set.empty

freeTypeVarsScheme :: Scheme -> Set Text
freeTypeVarsScheme (Scheme vars t) =
  Set.difference (freeTypeVars t) (Set.fromList vars)

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Text -> Type -> TI Substitution
varBind var ty
  | ty == TVar var = return emptySubst
  | var `Set.member` freeTypeVars ty = throwError "occurs check failed"
  | otherwise = return (Map.singleton var ty)

unify :: Type -> Type -> TI (Substitution, Type)
unify TInt TInt = return (emptySubst, TInt)
unify TBool TBool = return (emptySubst, TBool)
unify TChar TChar = return (emptySubst, TChar)
unify TString TString = return (emptySubst, TString)
unify TFloat TFloat = return (emptySubst, TFloat)
unify TThing TThing = return (emptySubst, TThing)
unify (TFun l r) (TFun l' r') = do
  (s1, tyl) <- unify l l'
  (s2, tyr) <- unify (applySubst s1 r) (applySubst s1 r')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs $ TFun tyl tyr)
unify (TPair a b) (TPair a' b') = do
  (s1, tyA) <- unify a a'
  (s2, tyB) <- unify (applySubst s1 b) (applySubst s1 b')
  let final_subs = s2 `composeSubst` s1
  return (final_subs, applySubst final_subs (TPair tyA tyB))
unify a@(TLabel id x) b@(TLabel id2 y) = do
  if id == id2
    then do
      (ctx, ty) <- unify x y
      return (ctx, TLabel id ty)
    else throwError ("types do not unify: " <> showT a <> " vs. " <> showT b)
unify (TVar u) t = do
  res <- varBind u t
  return (res, applySubst res t)
unify t (TVar u) = do
  res <- varBind u t
  return (res, applySubst res t)
unify a@(TSum x y) b = do
  res <- tryUnify x b
  case res of
    Nothing -> do
      res2 <- tryUnify y b
      case res2 of
        Nothing -> throwError ("types do not unify: " <> showT a <> " vs. " <> showT b)
        Just s' -> return s'
    Just s -> return s
  where
    tryUnify :: Type -> Type -> TI (Maybe (Substitution, Type))
    tryUnify a b = (Just <$> unify a b) `catchError` (\e -> return Nothing)
unify a b@(TSum x' y') = do
  res <- tryUnify a x'
  case res of
    Nothing -> do
      res2 <- tryUnify a y'
      case res2 of
        Nothing -> throwError ("types do not unify: " <> showT a <> " vs. " <> showT b)
        Just s' -> return s'
    Just s -> return s
    where
      tryUnify :: Type -> Type -> TI (Maybe (Substitution, Type))
      tryUnify a b = (Just <$> unify a b) `catchError` (\e -> return Nothing)
unify t1 t2 =
  throwError ("types do not unify: " <> showT t1 <> " vs. " <> showT t2)



type Context = Map Text Scheme

applySubstCtx :: Substitution -> Context -> Context
applySubstCtx subst = Map.map (applySubstScheme subst)

freeTypeVarsCtx :: Context -> Set Text
freeTypeVarsCtx ctx = foldMap freeTypeVarsScheme (Map.elems ctx)

generalize :: Context -> Type -> Scheme
generalize ctx t = Scheme vars t
  where
    vars = Set.toList (Set.difference (freeTypeVars t) (freeTypeVarsCtx ctx))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars ty) = do
  newVars <- traverse (const newTyVar) vars
  let subst = Map.fromList (zip vars newVars)
  return (applySubst subst ty)

inferLiteral :: Lit -> TI (Substitution, Type)
inferLiteral lit =
  return (emptySubst, case lit of
    LInt _ -> TInt
    LBool _ -> TBool
    LThing _ -> TThing
    LChar _ -> TChar
    LString _ -> TString
    LFloat _ -> TFloat)

inferPattern :: Pattern -> TI (Context, Type)
inferPattern (PLit binder) = do
  (_, tyBinder) <- inferLiteral binder
  return (Map.empty, tyBinder)
inferPattern (PVar binder) = do
  tyBinder <- newTyVar
  return (Map.singleton binder (Scheme [] tyBinder), tyBinder)
inferPattern (PPair a b) = do
  (ctxa, tya) <- inferPattern a
  (ctxb, tyb) <- inferPattern b
  return (Map.union ctxa ctxb, TPair tya tyb)
inferPattern (PLabel id x) = do
  (ctx, typ) <- inferPattern x
  return (ctx, TLabel id typ)
inferPattern (PAnnot typ x) = do
  (s1, typx) <- inferPattern x
  (_, final_typ) <- unify typ typx 
  return (s1, final_typ)

infer :: Context -> Exp -> TI (Substitution, Type)
infer ctx exp = case exp of
  EVar var -> case Map.lookup var ctx of
    Nothing ->
      throwError ("unbound variable: " <> showT var)
    Just scheme -> do
      ty <- instantiate scheme
      return (emptySubst, ty)
  ELit lit ->
    inferLiteral lit
  EApp fun arg -> do
    tyRes <- newTyVar
    (s1, tyFun) <- infer ctx fun
    (s2, tyArg) <- infer (applySubstCtx s1 ctx) arg
    (ctx, final_typ) <- unify (applySubst s2 tyFun) (TFun tyArg tyRes)
    let TFun a b = final_typ
    return (ctx, b)
  ELam pattern body -> do
    (ctx_p, tyP) <- inferPattern pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    return (s1, TFun (applySubst s1 tyP) tyBody)
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
        (final_subs, final_ty) <- unify (applySubst s2 tyA) tyB
        case final_ty of
          TFun _ _ -> return (final_subs, final_ty)
          other -> err
        `catchError` (\e -> do {
              let TFun ax ay = applySubst s2 tyA
            ; let TFun bx by = tyB
            ; (subs_x, tyX) <- unifyJoin ax bx 
            ; (subs_y, tyY) <- unifyJoin ay by 
            ; return (subs_x `composeSubst` subs_y, TFun tyX tyY)
          })
    where
      err = throwError $ T.pack ("Matches need to be functions: " <>  show m)
      unifyJoin a b = unify a b `catchError` (\e -> return (emptySubst, TSum a b))  
  ELet binder binding body -> do
    (s1, tyBinder) <- infer ctx binding
    let scheme = Scheme [] (applySubst s1 tyBinder)
    let tmpCtx = Map.insert binder scheme ctx
    (s2, tyBody) <- infer (applySubstCtx s1 tmpCtx) body
    return (s2 `composeSubst` s1, tyBody)
  EPair a b -> do
    (s1, tyA) <- infer ctx a
    (s2, tyB) <- infer (applySubstCtx s1 ctx) b
    return (s2 `composeSubst` s1, TPair (applySubst s2 tyA) tyB)
  ELabel id x -> do
    (s1, typ) <- infer ctx x
    return (s1, TLabel id typ)
  EAnnot typ x -> do
    (s1, typx) <- infer ctx x
    (fs, _) <- unify typ typx 
    return (fs `composeSubst` s1, typ)

typeInference :: Context -> Exp -> TI Type
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

testTI :: Exp -> IO ()
testTI e = do
  let (res, _) = runTI (typeInference primitives e)
  case res of
    Left err -> putStrLn $ show e ++ "\n " ++ Text.unpack err ++ "\n"
    Right t  -> putStrLn $ "\n" ++ Text.unpack (prettyScheme (generalize Map.empty t)) ++ "\n"

showT :: Show a => a -> Text
showT = Text.pack . show