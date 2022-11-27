{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CopiedMountain.Context where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Errors
import CopiedMountain.Data.Log

import qualified Data.Map.Strict as M
import Control.Monad.State (StateT (runStateT), MonadState (get, put), MonadTrans (lift))
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), MonadIO, MonadTrans, runExceptT)
import Control.Monad.Writer (WriterT (runWriterT), MonadWriter, MonadTrans)

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
  options :: Options,
  name_counter :: Int
} deriving (Show, Eq)

toList :: State -> [[(Id, Exp)]]
toList (State _ env _ _) = map M.toList env

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
  let (State u env' opt s) = env
  put $ State u (new_env : env') opt s

popEnv :: (Monad m) => ContextT m Env
popEnv = do
  (env :: State) <- get
  let (State u env' opt s) = env
  case env' of
    [] -> return M.empty
    (x:xs) -> do
      put $ State u xs opt s
      return x

getDef :: (Monad m) => Id -> ContextT m Exp
getDef id = do
  (env :: State) <- get
  let (State u env' _ _) = env
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
  let (State u env' _ _) = env
  return u

markChanged :: (Monad a) => ContextT a ()
markChanged = do
  (env :: State) <- get
  let (State _ env' opt s) = env
  put (State True env' opt s)

resetChanged :: (Monad a) => ContextT a ()
resetChanged = do
  (env :: State) <- get
  let (State _ env' opt s) = env
  put (State False env' opt s)

getOptions :: (Monad m) => ContextT m Options
getOptions = do
  (env :: State) <- get
  let State _ _ opt _ = env
  return opt

getEnv :: (Monad m) => ContextT m [Env]
getEnv = do
  (env :: State) <- get
  let State _ e _ _ = env
  return e

newTyVar :: (Monad m) => Bool -> ContextT m Type
newTyVar isNU = do
  State a b c s <- get
  put $ State a b c (s + 1)
  if isNU
    then return (TNUVar ("v" <> show s))
    else return (TVar ("u" <> show s))