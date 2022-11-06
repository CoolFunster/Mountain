{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
module CopiedMountain.Interpreter where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Log
import CopiedMountain.Data.Errors

import qualified Control.Monad.State.Strict as State
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Except
import Control.Monad.Writer.Strict
import Control.Monad.Extra (firstJustM, ifM)

import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace (trace)
import Data.Maybe (fromJust)

data Options = Options {
  parser :: FilePath -> IO Exp,
  repository :: FilePath,
  file_ext :: String
}
instance Eq Options where
  (==) a b = repository a == repository b && file_ext a == file_ext b
instance Show Options where
  show (Options _ r f) = show (r,f)

data State = State {
  changed :: Bool,
  env :: [Env],
  options :: Options
} deriving (Show, Eq)

toList :: State -> [[(Id, Exp)]]
toList (State _ env _) = map M.toList env

newtype ContextT m a = ContextT { getContext :: StateT State (ExceptT Error (WriterT [Log] m)) a}
  deriving (Functor, Applicative, Monad, MonadError Error, MonadWriter [Log], MonadState State, MonadIO)

runWith :: (Monad m) => State -> ContextT m a -> m (Either Error (a, State), [Log])
runWith env s = runWriterT (runExceptT (runStateT (getContext s) env))

instance MonadTrans ContextT where
    lift = ContextT . lift . lift . lift

isValid :: (Monad m) => ContextT m a -> ContextT m Bool
isValid c = do
  res <- c
  return True
  `catchError` (\e ->
    return False
  )

pushEnv :: (Monad m) => Env -> ContextT m ()
pushEnv new_env = do
  (env :: State) <- get
  let (State u env' opt) = env
  put $ State u (new_env : env') opt

popEnv :: (Monad m) => ContextT m Env
popEnv = do
  (env :: State) <- get
  let (State u env' opt) = env
  case env' of
    [] -> return M.empty
    (x:xs) -> do
      put $ State u xs opt
      return x

getDef :: (Monad m) => Id -> ContextT m Exp
getDef id = do
  (env :: State) <- get
  let (State u env' _) = env
  case env' of
    [] -> throwError $ UnboundId id
    (x:_) -> do
      case M.lookup id x of
        Nothing -> do
          popEnv
          res <- getDef id
          pushEnv x
          return res
        Just val -> return val

hasDef :: (Monad m) => Id -> ContextT m Bool
hasDef id = do
  getDef id
  return True
  `catchError` (\case
    UnboundId id -> return False
    other -> throwError other
  )

isChanged :: (Monad a) => ContextT a Bool
isChanged = do
  (env :: State) <- get
  let (State u env' _) = env
  return u

markChanged :: (Monad a) => ContextT a ()
markChanged = do
  (env :: State) <- get
  let (State _ env' opt) = env
  put (State True env' opt)

resetChanged :: (Monad a) => ContextT a ()
resetChanged = do
  (env :: State) <- get
  let (State _ env' opt) = env
  put (State False env' opt)

getOptions :: (Monad m) => ContextT m Options
getOptions = do
  (env :: State) <- get
  let State _ _ opt = env
  return opt

getEnv :: (Monad m) => ContextT m [Env]
getEnv = do
  (env :: State) <- get
  let State _ e _ = env
  return e

bind :: (Monad m) => Pattern -> Exp -> ContextT m Env
bind (PVar id) x = return $ M.singleton id x
bind a@(PLit l) b@(ELit l')
  | l == l' = return M.empty
  | otherwise = throwError $ BadBind a b
bind a@(PPair x y) b@(EPair x' y') = do
  res_a <- bind x x'
  res_b <- bind y y'
  return $ M.union res_a res_b
bind a@(PRecord omap) b@(ERecord omap') = do
  let k = M.keys omap
  foldr f (pure M.empty) k
  where
    f :: (Monad m) => Id -> ContextT m Env -> ContextT m Env
    f k cur_map' = do
      let e = M.lookup k omap'
      let p = fromJust $ M.lookup k omap
      cur_map <- cur_map'
      case e of
        Nothing -> throwError $ BadBind a b
        Just x -> do
          bres <- bind p x
          return $ M.union bres cur_map
bind a b = throwError $ BadBind a b


step :: (Monad m) => Exp -> ContextT m Exp
step t@(ELit _) = return t
step t@(EVar id) = do
  res <- getDef id
  markChanged
  return res
step t@(ELam _ _) = return t
step t@(EMatch x y) = return t
step t@(EApp a@(ELam pat y) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ EApp a res
    else do
      pat_e <- bind pat b
      e <- popEnv
      let e' = M.union pat_e e
      pushEnv e'
      markChanged
      return y
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
      else error "Should not reach here, bad typechecking on application"
step t@(ELet id x y) = do
  res_x <- step x
  c <- isChanged
  if c
    then return $ ELet id res_x y
    else do
      markChanged
      e <- popEnv
      let e' = M.insert id x e
      pushEnv e'
      return y
step t@(EPair a b) = do
  res <- step a
  c <- isChanged
  if c
    then return $ EApp res b
  else do
    res' <- step b
    c' <- isChanged
    if c'
      then return $ EApp a res'
      else return t
step t@(ERecord omap) = do
  let (ids, exps) = unzip $ M.toList omap
  exps' <- stepSeq exps
  return $ ERecord $ M.fromList (zip ids exps')
  where
    stepSeq :: (Monad m) => [Exp] -> ContextT m [Exp]
    stepSeq [] = return []
    stepSeq (x:xs) = do
      x' <- step x
      c <- isChanged
      if c 
        then return (x':xs)
        else do
          res <- stepSeq xs
          return (x:res)

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

