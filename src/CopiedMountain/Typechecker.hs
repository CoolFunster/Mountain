{-# language OverloadedStrings #-}
module CopiedMountain.Typechecker where

import Control.Monad (replicateM)
import Control.Monad.State (State, runState, get, put, gets, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import CopiedMountain.Data.AST

import Debug.Trace

type Substitution = Map Text Type

emptySubst :: Substitution
emptySubst = Map.empty

applySubst :: Substitution -> Type -> Type
applySubst subst ty = case ty of
  TPair a b -> TPair (applySubst subst a) (applySubst subst b)
  TVar var ->
    fromMaybe (TVar var) (Map.lookup var subst)
  TFun arg res ->
    TFun (applySubst subst arg) (applySubst subst res)
  TInt -> TInt
  TBool -> TBool

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

unify :: Type -> Type -> TI Substitution
unify TInt TInt = return emptySubst
unify TBool TBool = return emptySubst
unify (TFun l r) (TFun l' r') = do
  s1 <- unify l l'
  s2 <- unify (applySubst s1 r) (applySubst s1 r')
  return (s2 `composeSubst` s1)
unify (TPair a b) (TPair a' b') = do
  s1 <- unify a a'
  s2 <- unify (applySubst s1 b) (applySubst s1 b')
  return (s2 `composeSubst` s1)
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
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
    LBool _ -> TBool)

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
    s3 <- unify (applySubst s2 tyFun) (TFun tyArg tyRes)
    return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tyRes)
  ELam pattern body -> do
    (ctx_p, tyP) <- inferPattern pattern
    let tmpCtx = Map.union ctx_p ctx
    (s1, tyBody) <- infer tmpCtx body
    return (s1, TFun (applySubst s1 tyP) tyBody)
  m@(EMatch a b) -> do
    foo_type <- TFun <$> newTyVar <*> newTyVar
    (s1, tyA) <- infer ctx a
    (s2, tyB) <- infer (applySubstCtx s1 ctx) b
    s3 <- unify (applySubst s2 tyA) tyB
    let res_type = applySubst s3 tyB
    case res_type of
      TFun _ _ -> return (s3 `composeSubst` s2 `composeSubst` s1, applySubst s3 tyB)
      other -> error $ "Matches need to be functions: " ++ show m
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