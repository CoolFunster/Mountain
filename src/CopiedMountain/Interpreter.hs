{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module CopiedMountain.Interpreter where

import CopiedMountain.Hash
import CopiedMountain.Data.AST
import CopiedMountain.Data.Log
import CopiedMountain.Data.Errors
import CopiedMountain.Context

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Control.Monad.Extra (firstJustM, ifM)

import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe (fromJust)

unrollRec :: Exp -> Exp
unrollRec t@(ERec id x) = replace (M.singleton id t) x
unrollRec other = error $ "Cannot unroll non recursive obj " ++ show other

replace :: Env -> Exp -> Exp
replace env e@(ELit _) = e
replace env e@(EVar id) =
  case M.lookup id env of
    Nothing -> e
    Just x -> x
replace env (EApp a b) = EApp (replace env a) (replace env b)
replace env (ELam p b) = ELam p (replace env b)
replace env (EULam p b) = ELam p (replace env b)
replace env (ELet id a b) = ELet id (replace env a) (replace env b)
replace env (EULet id a b) = ELet id (replace env a) (replace env b)
replace env (EMatch a b) = EMatch (replace env a) (replace env b)
replace env (EPair a b) = EPair (replace env a) (replace env b)
replace env (ELabel id x) = ELabel id (replace env x)
replace env (EAnnot t x) = EAnnot t (replace env x)
replace env (ERec id x) = do
  let env' = M.withoutKeys env (S.singleton id)
  ERec id (replace env' x)
replace _ t@(ETDef _ _ _) = t
replace env (EUnique h a) = EUnique h (replace env a)

bind :: (Monad m) => Bool -> Pattern -> Exp -> ContextT m Env
bind u a@(PVar id) x =
  if u
    then return $ M.singleton id x
    else
      case x of
        u@(EUnique _ _) -> throwError $ UniqueInNonUniqueCtx a x
        other -> return $ M.singleton id x
bind _ a@(PLit l) b@(ELit l')
  | l == l' = return M.empty
  | otherwise = throwError $ BadBind a b
bind u a@(PPair x y) b@(EPair x' y') = do
  res_a <- bind u x x'
  res_b <- bind u y y'
  return $ M.union res_a res_b -- TODO Union intersection should be eq
bind u a@(PLabel id x) b@(ELabel id2 y) = do
  if id == id2
    then bind u x y
    else throwError $ BadBind a b
bind u a@(PAnnot typ x) b = bind u x b
bind _ (PUnique (PVar id)) b@(EUnique _ _) = return $ M.singleton id b
bind True a@(PUnique pat) (EUnique h x) = bind True pat x
bind False a@(PUnique pat) b@(EUnique h x) = throwError $ UniqueInNonUniqueCtx a b
bind _ a b = throwError $ BadBind a b

uniquify :: Exp -> IO Exp
uniquify t@(EVar _) = return t
uniquify t@(EApp a b) = do
  a' <- uniquify a
  b' <- uniquify b
  if any isUnique [a',b']
    then EUnique <$> randHash <*> pure (EApp a' b')
    else return (EApp a' b')
uniquify t@(ELam p e) = do
  e' <- uniquify e
  if isUnique e
    then EUnique <$> randHash <*> pure (ELam p e')
    else return (ELam p e')
uniquify t@(EULam p e) = do
  e' <- uniquify e
  if isUnique e
    then EUnique <$> randHash <*> pure (EULam p e')
    else return (EULam p e')
uniquify t@(ELet id a b) = do
  a' <- uniquify a
  b' <- uniquify b
  if any isUnique [a',b']
    then EUnique <$> randHash <*> pure (ELet id a' b')
    else return (ELet id a' b')
uniquify t@(EULet id a b) = do
  a' <- uniquify a
  b' <- uniquify b
  if any isUnique [a',b']
    then EUnique <$> randHash <*> pure (ELet id a' b')
    else return (ELet id a' b')
uniquify (EAnnot t e) = do
  e' <- uniquify e
  if isUnique e
    then EUnique <$> randHash <*> pure (EAnnot t e')
    else return (EAnnot t e')
uniquify t@(ETDef _ _ _) = return t
uniquify t@(ELit l) = return t
uniquify t@(ERec id e) = do
  e' <- uniquify e
  if isUnique e
    then EUnique <$> randHash <*> pure (ERec id e')
    else return (ERec id e')
uniquify t@(EMatch a b) = do
  a' <- uniquify a
  b' <- uniquify b
  if any isUnique [a',b']
    then EUnique <$> randHash <*> pure (EMatch a' b')
    else return (EMatch a' b')
uniquify t@(EPair a b) = do
  a' <- uniquify a
  b' <- uniquify b
  if any isUnique [a',b']
    then EUnique <$> randHash <*> pure (EPair a' b')
    else return (EPair a' b')
uniquify t@(ELabel id e) = do
  e' <- uniquify e
  if isUnique e
    then EUnique <$> randHash <*> pure (ELabel id e')
    else return (ELabel id e')
uniquify t@(EUnique h e) = return t

validate :: (Monad m) => Exp -> ContextT m Exp
validate t@(EVar _) = return t
validate t@(ELam p@(PUnique _) x) = throwError $ UniqueInNonUniqueCtx p t
validate t@(ELam pat x) = do
  _ <- validate x
  let pat_vars :: S.Set Id = patFreeVars pat
  let freeRefs :: M.Map Id Int = freeRefWithCounts x
  let has_non_u_use = any (\id ->
        case M.lookup id freeRefs of
            Nothing -> False
            Just n -> n > 1) pat_vars
  if has_non_u_use
    then return t
    else throwError $ ThisShouldBeUnique t
validate t@(EULam pat x) = do
  _ <- validate x
  let pat_vars :: S.Set Id = patFreeVars pat
  let freeRefs :: M.Map Id Int = freeRefWithCounts x
  let has_non_u_use = any (\id ->
        case M.lookup id freeRefs of
            Nothing -> False
            Just n -> n > 1) pat_vars
  if has_non_u_use
    then throwError $ ThisShouldBeNotUnique t
    else return t
validate t@(EApp a b) = EApp <$> validate a <*> validate b
validate t@(ELet p@(PUnique _) _ _) = throwError $ UniqueInNonUniqueCtx p t
validate t@(ELet pat a b) = do
  _ <- validate a
  _ <- validate b
  let pat_vars :: S.Set Id = patFreeVars pat
  let freeRefs :: M.Map Id Int = freeRefWithCounts b
  let has_non_u_use = any (\id ->
        case M.lookup id freeRefs of
            Nothing -> False
            Just n -> n > 1) pat_vars
  if has_non_u_use
    then return t
    else throwError $ ThisShouldBeUnique t
validate t@(EULet pat a b) = do
  _ <- validate a
  _ <- validate b
  let pat_vars :: S.Set Id = patFreeVars pat
  let freeRefs :: M.Map Id Int = freeRefWithCounts b
  let has_non_u_use = any (\id ->
        case M.lookup id freeRefs of
            Nothing -> False
            Just n -> n > 1) pat_vars
  if has_non_u_use
    then throwError $ ThisShouldBeNotUnique t
    else return t
validate (EAnnot t x) = EAnnot t <$> validate x
validate (ETDef id t e) = ETDef id t <$> validate e
validate t@(ELit l) = return t
validate t@(ERec id x) = do
  _ <- validate x
  if not (S.member id (freeRefs x))
    then throwError $ RecursiveBindNotUsed t
  else if isUnique x
    then throwError $ ThisShouldBeNotUnique t
    else return t
validate (EMatch a b) = EMatch <$> validate a <*> validate b
validate (EPair a b) = EPair <$> validate a <*> validate b
validate (ELabel id x) = ELabel id <$> validate x
validate (EUnique h x) = EUnique h <$> validate x

step :: Exp -> ContextT IO Exp
step x = do
  res <- step' x
  lift $ uniquify res
  where
    step' :: (Monad m) => Exp -> ContextT m Exp
    step' t@(ELit _) = return t
    step' t@(EVar id) = getDef id
    step' t@(ELam _ _) = return t
    step' t@(EULam _ _) = return t
    step' t@(EMatch x y) = return t
    step' t@(ERec x y) = return t
    step' t@(EUnique h x) = EUnique h <$> step' x
    step' (ETDef _ _ t) = do
      markChanged
      return t
    step' t@(EAnnot _ x) = do
      markChanged
      return x
    step' t@(EApp x@(ERec _ _) y) = do
      markChanged
      let res = unrollRec x
      return $ EApp res y
    step' t@(EApp a@(ELam pat y) b) = do
      res <- step' b
      c <- isChanged
      if c
        then return $ EApp a res
        else do
          pat_e <- bind False pat b
          markChanged
          return $ replace pat_e y
    step' t@(EApp a@(EULam pat y) b) = do
      res <- step' b
      c <- isChanged
      if c
        then return $ EApp a res
        else do
          pat_e <- bind True pat b
          markChanged
          return $ replace pat_e y
    step' t@(EApp a@(EMatch x y) b) = do
      step' (EApp x b)
      `catchError` (\case
        BadBind _ _ -> do
          markChanged
          return $ EApp y b
        other -> throwError other
      )
    step' t@(EApp a b) = do
      res <- step' a
      c <- isChanged
      if c
        then return $ EApp res b
      else do
        res' <- step' b
        c' <- isChanged
        if c'
          then return $ EApp a res'
          else error "Should not reach here, bad typechecking of calls"
    step' t@(ELet pat x y) = do
      res_x <- step' x
      c <- isChanged
      if c
        then return $ ELet pat res_x y
        else do
          pat_e <- bind False pat x
          markChanged
          return (replace pat_e y)
    step' t@(EULet pat x y) = do
      res_x <- step' x
      c <- isChanged
      if c
        then return $ ELet pat res_x y
        else do
          pat_e <- bind True pat x
          markChanged
          return (replace pat_e y)
    step' t@(EPair a b) = do
      res <- step' a
      c <- isChanged
      if c
        then return $ EPair res b
      else do
        res' <- step' b
        c' <- isChanged
        if c'
          then return $ EPair a res'
          else return t
    step' t@(ELabel id exp) = ELabel id <$> step' exp

evaluate :: Maybe Int -> Exp -> ContextT IO Exp
evaluate (Just 0) x = return x
evaluate count t
  | (Just x) <- count, x < 0 = error "negative value for num steps"
  | otherwise = do
      res <- step t
      env <- getEnv
      tell [Step res env]
      c <- isChanged
      if c
        then do
          resetChanged
          evaluate (Just (\x -> x - 1) <*> count) res
        else return res

preprocess :: Exp -> ContextT IO Exp
preprocess x = do
  ux <- lift (uniquify x)
  validate ux