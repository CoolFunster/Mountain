{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimpleMountain.SimpleMountain where

import qualified Data.Map as M
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer.Strict hiding (Any, All)
import GHC.Base (Nat)
import qualified Data.Set as S
import System.Directory (doesFileExist)

type Id = [Char]

type Env a = M.Map Id a

data Structure a =
    Extern a
  | Var Id
  | Function (Structure a) (Structure a)
  | Call (Structure a) (Structure a)
  | Let Id (Structure a) (Structure a)
  | Context (Env (Structure a)) (Structure a)
  | Import Id String (Structure a)
  deriving (Eq, Show)

data Error a =
    NotImplemented String (Structure a)
  | UnboundId Id
  | BadCall (Structure a) (Structure a)
  | BadFunctionDef (Structure a) (Structure a)
  | BadCallDef (Structure a) (Structure a)
  | BadBind (Structure a) (Structure a)
  | BadAssert (Structure a)
  | BadAssertIs (Structure a) (Structure a)
  | BadImport String
  | ImportHasFreeVars String
  deriving (Show, Eq)

data Log a =
  Step (Structure a) [Env (Structure a)]
  deriving (Eq)

data MountainExtern =
    Thing String
  | Bool Bool
  | Char Char
  | String String
  | Int Int
  | Float Float
  | Unit
  | Empty
  | Assert
  | AssertFail
  | AssertIs
  | Is
  deriving (Eq, Show)

type MountainError = Error MountainExtern
type MountainLog = Log MountainExtern
type MountainTerm = Structure MountainExtern
data MountainOptions = Options {
  parser :: FilePath -> IO MountainTerm,
  repository :: FilePath,
  file_ext :: String
}
instance Eq MountainOptions where
  (==) a b = repository a == repository b && file_ext a == file_ext b
instance Show MountainOptions where
  show (Options _ r f) = show (r,f)
data MountainState = MountainState {
  changed :: Bool,
  env :: [Env MountainTerm],
  options :: MountainOptions
} deriving (Show, Eq)

toList :: MountainState -> [[(Id, MountainTerm)]]
toList (MountainState _ env _) = map M.toList env

-- This just lets me throw errors, write to a log, and manage bindings without explicitly writing all the boilerplate. 
newtype MountainContextT m a = MountainContextT { getContext :: StateT MountainState (ExceptT MountainError (WriterT [MountainLog] m)) a}
  deriving (Functor, Applicative, Monad, MonadError MountainError, MonadWriter [MountainLog], MonadState MountainState, MonadIO)

runMountainContextT :: MountainState -> MountainContextT m a -> m (Either MountainError (a, MountainState), [MountainLog])
runMountainContextT env s = runWriterT (runExceptT (runStateT (getContext s) env))

instance MonadTrans MountainContextT where
    lift = MountainContextT . lift . lift . lift

isValid :: (Monad m) => MountainContextT m a -> MountainContextT m Bool
isValid c = do
  res <- c
  return True
  `catchError` (\e ->
    return False
  )

pushEnv :: (Monad m) => Env MountainTerm -> MountainContextT m ()
pushEnv new_env = do
  (env :: MountainState) <- get
  let (MountainState u env' opt) = env
  put $ MountainState u (new_env : env') opt

popEnv :: (Monad m) => MountainContextT m (Env MountainTerm)
popEnv = do
  (env :: MountainState) <- get
  let (MountainState u env' opt) = env
  case env' of
    [] -> return M.empty
    (x:xs) -> do
      put $ MountainState u xs opt
      return x

getDef :: (Monad m) => Id -> MountainContextT m MountainTerm
getDef id = do
  (env :: MountainState) <- get
  let (MountainState u env' _) = env
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

hasDef :: (Monad m) => Id -> MountainContextT m Bool
hasDef id = do
  getDef id
  return True
  `catchError` (\case
    UnboundId id -> return False
    other -> throwError other
  )

isChanged :: (Monad a) => MountainContextT a Bool
isChanged = do
  (env :: MountainState) <- get
  let (MountainState u env' _) = env
  return u

markChanged :: (Monad a) => MountainContextT a ()
markChanged = do
  (env :: MountainState) <- get
  let (MountainState _ env' opt) = env
  put (MountainState True env' opt)

resetChanged :: (Monad a) => MountainContextT a ()
resetChanged = do
  (env :: MountainState) <- get
  let (MountainState _ env' opt) = env
  put (MountainState False env' opt)

getOptions :: (Monad m) => MountainContextT m MountainOptions
getOptions = do
  (env :: MountainState) <- get
  let MountainState _ _ opt = env
  return opt

getEnv :: (Monad m) => MountainContextT m [Env MountainTerm]
getEnv = do
  (env :: MountainState) <- get
  let MountainState _ e _ = env
  return e

freeVars :: MountainTerm -> S.Set Id
freeVars (Extern _) = S.empty
freeVars (Var id) = S.singleton id
freeVars (Function a b) = S.difference (freeVars b) (freeVars a)
freeVars (Call a b) = S.union (freeVars a) (freeVars b)
freeVars (Let id a b) = S.difference (S.union (freeVars a) (freeVars b)) (S.singleton id)
freeVars (Context env a) = S.difference (freeVars a) (S.fromList (M.keys env))
freeVars (Import id str x) = freeVars x -- because imports should not have any free vars

validate :: (Monad m) => MountainTerm -> MountainContextT m MountainTerm
validate t@(Extern _) = return t
validate t@(Var _) = return t
validate (Function a@(Var _) b) = Function a <$> validate b
validate (Function other b) = throwError $ BadFunctionDef other b
validate (Call a b) = Call <$> validate a <*> validate b
validate (Let id x y) = Let id <$> validate x <*> validate y
validate (Context env a) = Context <$> mapM validate env <*> validate a
validate (Import id str x) = Import id str <$> validate x

-- normalize :: MountainTerm -> MountainTerm
-- normalize t@(Extern _) = t
-- normalize t@(Var _) = t
-- normalize t@(Function a b) = Function (normalize a) (normalize b)
-- normalize t@(Call a b) = Call (normalize a) (normalize b)
-- normalize t@(Let id x y) = Let id (normalize x) (normalize y)
-- normalize t@(Context env (Context env' x)) = normalize $ Context (M.union env' env) x
-- normalize t@(Context env x)
--   | M.null env = normalize x
--   | otherwise = Context (M.map normalize env) (normalize x)

bind :: MountainTerm -> MountainTerm -> MountainContextT IO (Env MountainTerm)
bind (Var id) t = return $ M.singleton id t
bind (Function a b) (Function a' b') = M.union <$> bind a a' <*> bind b b'
bind other t = throwError $ BadBind other t

dotPathAsDir :: String -> FilePath
dotPathAsDir [] = []
dotPathAsDir ('.':xs) = '/':dotPathAsDir xs
dotPathAsDir (x:xs) = x:dotPathAsDir xs

dirAsDotPath :: String -> FilePath
dirAsDotPath [] = []
dirAsDotPath ('/':xs) = '.':dirAsDotPath xs
dirAsDotPath (x:xs) = x:dirAsDotPath xs

step :: MountainTerm -> MountainContextT IO MountainTerm
step t@(Extern _) = return t
step t@(Var id) = do
  res <- getDef id
  markChanged
  return res
step t@(Function _ _) = return t
step t@(Call (Extern Assert) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ Call (Extern Assert) res
    else do
      case b of
        (Extern (Bool True)) -> return (Extern Unit)
        _ -> throwError $ BadAssert b
step t@(Call (Call (Extern Is) a) b) = do
  res_a <- step a
  c <- isChanged
  if c
    then return (Call (Call (Extern Is) res_a) b)
  else do
    res_b <- step b
    c2 <- isChanged
    if c2
      then return (Call (Call (Extern Is) a) res_b)
    else if a == b
      then do
        markChanged
        return $ Extern (Bool True)
      else do
        markChanged
        return $ Extern (Bool False)
step t@(Call (Call (Extern AssertIs) a) b) = do
  res_a <- step a
  c <- isChanged
  if c
    then return (Call (Call (Extern AssertIs) res_a) b)
  else do
    res_b <- step b
    c2 <- isChanged
    if c2
      then return (Call (Call (Extern AssertIs) a) res_b)
    else if a == b
      then do
        markChanged
        return $ Extern Unit
      else throwError $ BadAssertIs a b
step t@(Call (Extern AssertFail) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ Call (Extern AssertFail) res
    else throwError $ BadAssert b
  `catchError` (\e -> do
    markChanged
    return $ Extern Unit)
step t@(Call x@(Function a@(Var id) y) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ Call x res
    else do
      res <- bind a b
      markChanged
      return $ Context res y
step t@(Call a b) = do
  res <- step a
  c <- isChanged
  if c
    then return $ Call res b
  else do
    res' <- step a
    c' <- isChanged
    if c'
      then return $ Call a res'
      else throwError $ BadCall a b
step t@(Let id x y) = do
  res_x <- step x
  c <- isChanged
  if c
    then return $ Let id res_x y
    else do
      markChanged
      return $ Context (M.singleton id x) y
  `catchError` (\e -> return $ Extern Unit)
step t@(Context e s) = do
  pushEnv e
  res <- step s
  e' <- popEnv
  prepruned <-
        case res of
              (Context another_e x) -> do
                markChanged
                return $ Context (M.union another_e e') res
              other -> return $ Context e' other
  c <- isChanged
  if c
    then return prepruned
    else do
      let Context e t = prepruned
      let free = freeVars s
      let pruned =
            foldr (\id new_env -> case M.lookup id e of
                  Nothing -> new_env
                  Just struc -> M.insert id struc new_env
                ) M.empty free
      if M.null pruned
        then do
          markChanged
          return t
        else do
          let final = Context pruned t
          if final /= prepruned
            then do
              markChanged
              return $ Context pruned t
            else
              return $ Context pruned t
step (Import id dot_path t) = do
  opt <- getOptions
  let Options parser repo file_ext = opt
  let full_path = repo ++ dotPathAsDir dot_path ++ file_ext
  file_exist <- lift $ doesFileExist full_path
  if not file_exist
    then throwError $ BadImport dot_path
    else do
      res <- lift $ parser full_path
      if freeVars res /= S.empty
        then throwError $ ImportHasFreeVars dot_path
        else do
          markChanged
          return $ Let id res t

evaluate :: Maybe Int -> MountainTerm -> MountainContextT IO MountainTerm
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

execute :: Maybe Int -> MountainTerm -> MountainContextT IO MountainTerm
execute c t = validate t >>= evaluate c