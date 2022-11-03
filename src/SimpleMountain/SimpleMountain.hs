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
    Literal a
  -- | Refs must have definitions (via function or let) before use
  | Ref Id  
  -- | Holes are placeholders to be filled in. Errors on eval. Meant to be solved for
  | Hole Id 
  | Function (Structure a) (Structure a)
  | Let Id (Structure a) (Structure a)
  | Call (Structure a) (Structure a)
  | Context (Env (Structure a)) (Structure a)
  deriving (Eq, Show)

type MountainTerm = Structure MountainLiteral
data MountainLiteral =
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

type MountainError = Error MountainLiteral
data Error a =
    NotImplemented String
  | UnboundId Id
  | BadCall (Structure a) (Structure a)
  | BadFunctionDef (Structure a) (Structure a)
  | BadCallDef (Structure a) (Structure a)
  | BadBind (Structure a) (Structure a)
  | BadAssert (Structure a)
  | BadAssertIs (Structure a) (Structure a)
  | EvaluateHole Id
  deriving (Show, Eq)

type MountainLog = Log MountainLiteral
data Log a =
  Step (Structure a) [Env (Structure a)]
  deriving (Eq)

data MountainOptions = Options {
  parser :: FilePath -> IO MountainTypedTerm,
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

freeRefs :: MountainTerm -> S.Set Id
freeRefs (Literal _) = S.empty
freeRefs (Ref id) = S.singleton id
freeRefs (Hole _) = S.empty
freeRefs (Function a b) = S.difference (freeRefs b) (freeRefs a)
freeRefs (Call a b) = S.union (freeRefs a) (freeRefs b)
freeRefs (Let id a b) = S.difference (S.union (freeRefs a) (freeRefs b)) (S.singleton id)
freeRefs (Context env a) = S.difference (freeRefs a) (S.fromList (M.keys env))

-- normalize :: MountainTerm -> MountainTerm
-- normalize t@(Literal _) = t
-- normalize t@(Ref _) = t
-- normalize t@(Function a b) = Function (normalize a) (normalize b)
-- normalize t@(Call a b) = Call (normalize a) (normalize b)
-- normalize t@(Let id x y) = Let id (normalize x) (normalize y)
-- normalize t@(Context env (Context env' x)) = normalize $ Context (M.union env' env) x
-- normalize t@(Context env x)
--   | M.null env = normalize x
--   | otherwise = Context (M.map normalize env) (normalize x)

bind :: (Monad m) => MountainTerm -> MountainTerm -> MountainContextT m (Env MountainTerm)
bind (Ref id) t = return $ M.singleton id t
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

step :: (Monad m) => MountainTerm -> MountainContextT m MountainTerm
step t@(Literal _) = return t
step t@(Ref id) = do
  res <- getDef id
  markChanged
  return res
step t@(Hole n) = throwError $ EvaluateHole n
step t@(Function _ _) = return t
step t@(Call (Literal Assert) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ Call (Literal Assert) res
    else do
      case b of
        (Literal (Bool True)) -> return (Literal Unit)
        _ -> throwError $ BadAssert b
step t@(Call (Call (Literal Is) a) b) = do
  res_a <- step a
  c <- isChanged
  if c
    then return (Call (Call (Literal Is) res_a) b)
  else do
    res_b <- step b
    c2 <- isChanged
    if c2
      then return (Call (Call (Literal Is) a) res_b)
    else if a == b
      then do
        markChanged
        return $ Literal (Bool True)
      else do
        markChanged
        return $ Literal (Bool False)
step t@(Call (Call (Literal AssertIs) a) b) = do
  res_a <- step a
  c <- isChanged
  if c
    then return (Call (Call (Literal AssertIs) res_a) b)
  else do
    res_b <- step b
    c2 <- isChanged
    if c2
      then return (Call (Call (Literal AssertIs) a) res_b)
    else if a == b
      then do
        markChanged
        return $ Literal Unit
      else throwError $ BadAssertIs a b
step t@(Call (Literal AssertFail) b) = do
  res <- step b
  c <- isChanged
  if c
    then return $ Call (Literal AssertFail) res
    else throwError $ BadAssert b
  `catchError` (\e -> do
    markChanged
    return $ Literal Unit)
step t@(Call x@(Function a@(Ref id) y) b) = do
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
  `catchError` (\e -> return $ Literal Unit)
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
      let free = freeRefs s
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

evaluate :: (Monad m) => Maybe Int -> MountainTerm -> MountainContextT m MountainTerm
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

-------------------------
-- Mountain with Types
-------------------------

type MountainType = Structure MountainTypes
data MountainTypes =
    Things
  | Bools
  | Chars
  | Strings
  | Ints
  | Floats
  | UnitType
  | EmptyType
  deriving (Eq, Show)

type MountainTypedTerm = TypedStructure MountainTypes MountainLiteral
data TypedStructure mType mLit = 
    Term (Structure mLit)
  | Typed (Structure mType) (Structure mLit)
  deriving (Eq, Show)

typecheck :: (Monad m) => MountainTypedTerm -> MountainContextT m MountainTerm
typecheck (Term s) = return s
typecheck t@(Typed _ _) = throwError $ NotImplemented $ "typechecking: " ++ show t

execute :: (Monad m) => Maybe Int -> MountainTypedTerm -> MountainContextT m MountainTerm
execute c t = typecheck t >>= evaluate c 
