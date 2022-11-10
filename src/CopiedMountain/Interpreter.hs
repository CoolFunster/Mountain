{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module CopiedMountain.Interpreter where

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
replace env (ELet id a b) = ELet id (replace env a) (replace env b)
replace env (EMatch a b) = EMatch (replace env a) (replace env b)
replace env (EPair a b) = EPair (replace env a) (replace env b)
replace env (ELabel id x) = ELabel id (replace env x)
replace env (EAnnot t x) = EAnnot t (replace env x)
replace env (ERec id x) = do
  let env' = M.withoutKeys env (S.singleton id)
  ERec id (replace env' x)
replace _ t@(ETDef _ _ _) = t
-- replace env (EUnique h a) = EUnique h (replace env a)

bind :: (Monad m) => Pattern -> Exp -> ContextT m Env
-- bind a@(PVar id) b@(EUnique _ _) = throwError $ BadBind a b
bind (PVar id) x = return $ M.singleton id x
bind a@(PLit l) b@(ELit l')
  | l == l' = return M.empty
  | otherwise = throwError $ BadBind a b
bind a@(PPair x y) b@(EPair x' y') = do
  res_a <- bind x x'
  res_b <- bind y y'
  return $ M.union res_a res_b
bind a@(PLabel id x) b@(ELabel id2 y) = do
  if id == id2
    then bind x y
    else throwError $ BadBind a b
bind a@(PAnnot typ x) b = bind x b
-- bind (PUnique (PVar id)) b@(EUnique _ _) = return $ M.singleton id b
-- bind a@(PUnique pat) (EUnique h x) = bind pat x
bind a b = throwError $ BadBind a b


step :: (Monad m) => Exp -> ContextT m Exp
step t@(ELit _) = return t
step t@(EVar id) = getDef id
step t@(ELam _ _) = return t
step t@(EMatch x y) = return t
step t@(ERec x y) = return t
-- step t@(EUnique h x) = EUnique h <$> step x
step (ETDef _ _ t) = do
  markChanged
  return t
step t@(EAnnot _ x) = do
  markChanged
  return x
step t@(EApp x@(ERec _ _) y) = do
  markChanged
  let res = unrollRec x
  return $ EApp res y
step t@(EApp a@(ELam pat y) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ EApp a res
    else do
      pat_e <- bind pat b
      markChanged
      return $ replace pat_e y
step t@(EApp a@(EMatch x y) b) = do
  step (EApp x b)
  `catchError` (\case
    BadBind _ _ -> do
      markChanged
      return $ EApp y b
    other -> throwError other
  )
step t@(EApp a b) = do
  res <- step a
  c <- isChanged
  if c
    then return $ EApp res b
  else do
    res' <- step b
    c' <- isChanged
    if c'
      then return $ EApp a res'
      else error "Should not reach here, bad typechecking of calls"
step t@(ELet pat x y) = do
  res_x <- step x
  c <- isChanged
  if c
    then return $ ELet pat res_x y
    else do
      pat_e <- bind pat x
      markChanged
      return (replace pat_e y)
step t@(EPair a b) = do
  res <- step a
  c <- isChanged
  if c
    then return $ EPair res b
  else do
    res' <- step b
    c' <- isChanged
    if c'
      then return $ EPair a res'
      else return t
step t@(ELabel id exp) = ELabel id <$> step exp

evaluate :: (Monad m) => Maybe Int -> Exp -> ContextT m Exp
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

