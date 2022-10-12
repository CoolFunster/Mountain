{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mountain where

import Hash
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Monad.Writer.Strict hiding (Any, All)
import Control.Monad.Identity
import GHC.Base (returnIO)
import System.Directory
import Data.List
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Debug.Trace (trace)


type Id = [Char]

type Env a = M.Map Id a

data Structure a =
    Literal a
  | Wildcard
  | Reference Id
  | Set [Structure a] -- TODO MAKE THIS AN ACTUAL SET
  | Tuple [Structure a]
  | Function [Structure a]
  | Either [Structure a]
  | Each [Structure a]
  | Except [Structure a]
  | Scope [Structure a]
  | Import (Structure a)
  | Refine (Structure a) (Structure a)
  | Unique Hash (Structure a)
  | Call (Structure a) (Structure a)
  | Has (Structure a) (Structure a)
  | Bind (Structure a) (Structure a)
  | Recursive Id (Structure a)
  | Select (Structure a) [Id]
  | Hide (Structure a) [Id]
  | Context (Env (Structure a)) (Structure a)
  deriving (Eq, Show)

data Error a =
  NotImplemented String (Structure a) |
  NotNormalized (Structure a) |
  EmptyEither |
  EmptyEach |
  UnboundId Id |
  EmptyScope |
  BadImport (Structure a) |
  BadCall (Structure a) (Structure a) |
  BadHas (Structure a) (Structure a) |
  BadBind (Structure a) (Structure a) |
  BadSelect (Structure a) [Id]
  deriving (Show, Eq)

data Log a =
    -- ResolvedRef MountainTerm MountainTerm |
  Step (Structure a) [Env (Structure a)]
  deriving (Eq)

-- ################## --
-- HELPER FUNCTIONS   --
-- ################## --

isCall :: Structure a -> Bool
isCall (Call _ _) = True
isCall _ = False

isHas :: Structure a -> Bool
isHas (Has _ _) = True
isHas _ = False


-- ################## --
-- USEFUL STRUCTURES  --
-- ################## --

unit :: Structure a
unit = Tuple []

nothing :: Structure a
nothing = Either []

empty :: Structure a
empty = Set []

bool :: Structure a
bool = Set [nothing, unit]

-- ################## --
-- STRUCTURAL HELPERS --
-- ################## --
instance Functor Structure where
  fmap f (Literal x) = Literal (f x)
  fmap f Wildcard = Wildcard
  fmap f (Reference id) = Reference id
  fmap f (Import x) = Import (fmap f x)
  fmap f (Set x) = Set $ map (fmap f) x
  fmap f (Either x) = Either $ map (fmap f) x
  fmap f (Tuple x) = Tuple $ map (fmap f) x
  fmap f (Function x) = Function $ map (fmap f) x
  fmap f (Each x) = Each $ map (fmap f) x
  fmap f (Except x) = Each $ map (fmap f) x
  fmap f (Scope x) = Scope $ map (fmap f) x
  fmap f (Call x y) = Call (fmap f x) (fmap f y)
  fmap f (Refine x y) = Refine (fmap f x) (fmap f y)
  fmap f (Has x y) = Has (fmap f x) (fmap f y)
  fmap f (Bind x y) = Bind (fmap f x) (fmap f y)
  fmap f (Recursive id y) = Recursive id (fmap f y)
  fmap f (Select x ids) = Select (fmap f x) ids
  fmap f (Hide x ids) = Select (fmap f x) ids
  fmap f (Unique h x) = Unique h (fmap f x)
  fmap f (Context env x) = Context (M.map (fmap f) env) (fmap f x)

data ShouldRecurse a =
  Result a |
  Recurse
  deriving (Eq, Show)

transform :: (Structure a -> ShouldRecurse (Structure a)) -> Structure a -> Structure a
transform transformer input_structure = do
  case transformer input_structure of
    Result s -> s
    Recurse -> do
      let recurse = transform transformer
      case input_structure of
        (Literal x) -> Literal x
        Wildcard -> Wildcard
        (Set ic) -> Set $ map recurse ic
        (Unique h ic) -> Unique h $ recurse ic
        (Function xs) -> Function $ map recurse xs
        (Each xs) -> Each $ map recurse xs
        (Tuple xs) -> Tuple $ map recurse xs
        (Either xs) -> Either $ map recurse xs
        (Except xs) -> Either $ map recurse xs
        (Refine b p) -> Refine (recurse b) (recurse p)
        (Call b a) -> Call (recurse b) (recurse a)
        (Import i_c) -> Import (recurse i_c)
        (Has b s) -> Has (recurse b) (recurse s)
        (Scope xs) -> Scope $ map recurse xs
        (Bind a b) -> Bind (recurse a) (recurse b)
        (Recursive id x) -> Recursive id (recurse x)
        (Reference id) -> Reference id
        (Select a ids) -> Select (recurse a) ids
        (Hide a ids) -> Hide (recurse a) ids
        (Context env a) -> Context (M.map recurse env) (recurse a)

check :: ([Bool] -> Bool) -> (Structure a -> (Bool, ShouldRecurse (Structure a))) -> Structure a -> Bool
check f c s = do
  case c s of
    (b, Result _) -> b
    (b, Recurse) -> do
      let recurse = check f c
      case s of
        (Literal x) -> b
        Wildcard -> b
        (Set ic) -> f (b:map recurse ic)
        (Unique h ic) -> f [b,recurse ic]
        (Function xs) -> f (b:map recurse xs)
        (Each xs) -> f (b:map recurse xs)
        (Tuple xs) -> f (b:map recurse xs)
        (Either xs) -> f (b:map recurse xs)
        (Except xs) -> f (b:map recurse xs)
        (Refine x y) -> f [b,recurse x,recurse y]
        (Call x y) -> f [b,recurse x,recurse y]
        (Import i_c) -> f [b, recurse i_c]
        (Has x y) -> f [b,recurse x,recurse y]
        (Scope xs) -> f (b:map recurse xs)
        (Bind x y) -> f [b,recurse x,recurse y]
        (Recursive id x) -> f [b, recurse x]
        (Reference id) -> b
        (Select a ids) -> f [b,recurse a]
        (Hide a ids) -> f [b,recurse a]
        (Context env a) -> f (b:recurse a:M.elems (M.map recurse env))

checkAny :: (Structure a -> (Bool, ShouldRecurse (Structure a))) -> Structure a -> Bool
checkAny = check or

replace :: (Eq a) => Structure a -> Structure a -> Structure a -> Structure a
replace old new input_s =
  let
    replaceOldWithNew :: (Eq a) => Structure a -> Structure a -> ShouldRecurse (Structure a)
    replaceOldWithNew old_c new_c =
      if new_c == old_c
        then Result new_c
        else Recurse
  in
    transform (replaceOldWithNew old) input_s

-- TODO handle tuples and functions in a dep context with subtle binds
-- probably via star
-- TODO isRecursive should handle Contexts
isRecursive :: Structure a -> Bool
isRecursive (Bind (Reference n) s) =
  let
    hasRefOfn :: Structure a -> (Bool, ShouldRecurse (Structure a))
    hasRefOfn r@(Reference k) = (n == k, Result r)
    hasRefOfn b@(Bind (Reference k) v) =
      if n == k
        then (False, Result b)
        else (False, Recurse)
    hasRefOfn k = (False, Recurse)
  in
    checkAny hasRefOfn s
isRecursive _ = False

-- ############################# --
-- Basic Structure functionality --
-- ############################# --

instance (Show a, Hashable a) => Hashable (Structure a) where
  hash (Literal x)= hash x
  hash Wildcard = hashStr "?"
  hash (Reference id) = seqHash [hashStr "Reference", hashStr id]
  hash (Unique h x) = seqHash [hashStr "Unique", h, hash x]
  hash (Import a) = seqHash [hashStr "Import", hash a]
  hash (Set xs) = seqHash [hashStr "Set", sumHash (map hash xs)]
  hash (Either xs) = seqHash [hashStr "Either", sumHash (map hash xs)]
  hash (Except xs) = seqHash [hashStr "Except", sumHash (map hash xs)]
  hash (Call a b) = seqHash [hashStr "Call", hash a, hash b]
  hash (Refine a b) = seqHash [hashStr "Refine", hash a, hash b]
  hash (Has a b) = seqHash [hashStr "Has", hash a, hash b]
  hash (Bind a b) = seqHash [hashStr "Bind", hash a, hash b]
  hash (Recursive id b) = seqHash [hashStr "Recursive", hashStr id, hash b]
  hash (Select a ids) = seqHash [hashStr "Select", hash a, seqHash $ map hashStr ids]
  hash (Hide a ids) = seqHash [hashStr "Hide", hash a, seqHash $ map hashStr ids]
  hash (Function xs) = seqHash $ hashStr "Function" : map hash xs
  hash (Each xs) = seqHash $ hashStr "Each" : map hash xs
  hash (Tuple xs) = seqHash $ hashStr "Tuple" : map hash xs
  hash (Scope xs) = seqHash $ hashStr "Scope" : map hash xs
  hash (Context env a) = do
    let hash_env = M.map hash env
    let hashed_env = hashStr $ show hash_env
    seqHash [hashStr "Context",  hashed_env, hash a]

-- ############################# --
--    Mountain  Stuff            --
-- ############################# --

-- hashing 
-- TODO: ensure "Any, ?" are reserved keywords in parser
data MountainLiteral =
    Thing String
  | Bool Bool
  | Char Char
  | String String
  | Int Int
  | Float Float
  | All
  deriving (Eq, Show)

instance Hashable MountainLiteral where
  hash (Thing name) = hashStr $ "_" ++ name
  hash (Bool b) = hashStr $ "_" ++ show b
  hash (Char c) = hashStr [c]
  hash (String s) = hashStr $ "_" ++ s
  hash (Int i) = hashStr $ "_" ++ show i
  hash (Float f) = hashStr $ "_" ++ show f
  hash All = hashStr "All"

type MountainTerm = Structure MountainLiteral

data MountainOptions = Options {
  parser :: FilePath -> IO MountainTerm,
  repository :: FilePath,
  file_ext :: String
}

instance Eq MountainOptions where
  (==) a b = repository a == repository b && file_ext a == file_ext b

instance Show MountainOptions where
  show (Options _ r f) = show (r,f)

data MountainEnv = MountainEnv {
    options :: MountainOptions,
    environment :: [Env MountainTerm]
  } deriving (Eq, Show)

toList :: MountainEnv -> [[(Id, MountainTerm)]]
toList (MountainEnv _ env) = map M.toList env

dummyEnv :: MountainEnv
dummyEnv = MountainEnv (Options (const $ return empty) "" "") []

type MountainError = Error MountainLiteral

type MountainLog = Log MountainLiteral

-- This just lets me throw errors, write to a log, and manage bindings without explicitly writing all the boilerplate. 
newtype MountainContextT m a = MountainContextT { getContext :: StateT MountainEnv (ExceptT MountainError (WriterT [MountainLog] m)) a}
  deriving (Functor, Applicative, Monad, MonadError MountainError, MonadWriter [MountainLog], MonadState MountainEnv, MonadIO)

runMountainContextT :: MountainEnv -> MountainContextT m a -> m (Either MountainError (a, MountainEnv), [MountainLog])
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
  (env :: MountainEnv) <- get
  let MountainEnv a b = env
  put $ MountainEnv a (new_env : b)

popEnv :: (Monad m) => MountainContextT m (Env MountainTerm)
popEnv = do
  (env :: MountainEnv) <- get
  let MountainEnv a b = env
  case b of
    [] -> return M.empty
    (x:xs) -> do
      put $ MountainEnv a xs
      return x

hasDef :: (Monad m) => Id -> MountainContextT m Bool
hasDef id = do
  getDef id
  return True
  `catchError` (\case
    UnboundId id -> return False
    other -> throwError other
  )

maybeGetDef :: (Monad m) => Id -> MountainContextT m (Maybe MountainTerm)
maybeGetDef id = do
  Just <$> getDef id
  `catchError` (\e -> return Nothing)

getDef :: (Monad m) => Id -> MountainContextT m MountainTerm
getDef id = do
  (env :: MountainEnv) <- get
  let MountainEnv a b = env
  case b of
    [] -> throwError $ UnboundId id
    (x:_) -> do
      case M.lookup id x of
        Nothing -> do
          popEnv
          res <- getDef id
          pushEnv x
          return res
        Just u@(Unique h x) -> do
          x <- popEnv
          let x' = M.delete id x
          pushEnv x'
          return u
        Just val -> return val

putDef :: (Monad m) => Id -> MountainTerm -> MountainContextT m ()
putDef id v = do
  (env :: MountainEnv) <- get
  let MountainEnv a b = env
  case b of
    [] -> put $ MountainEnv a [M.singleton id v]
    (m:ms) -> put $ MountainEnv a (M.insert id v m:ms)

unionEnv :: (Monad m) => Env MountainTerm -> MountainContextT m ()
unionEnv new_env = do
  (env :: MountainEnv) <- get
  let MountainEnv a b = env
  case b of
    [] -> put $ MountainEnv a [new_env]
    (m:ms) -> put $ MountainEnv a (M.union new_env m:ms)

getOptions :: (Monad m) => MountainContextT m MountainOptions
getOptions = do
  (env :: MountainEnv) <- get
  let MountainEnv opt _ = env
  return opt

getEnv :: (Monad m) => MountainContextT m [Env MountainTerm]
getEnv = do
  (env :: MountainEnv) <- get
  let MountainEnv _ e = env
  return e

getBoundIds :: [Id] -> MountainContextT IO ([MountainTerm], [Id])
getBoundIds = foldr get_bound_ids (return ([],[]))
    where
      get_bound_ids :: Id -> MountainContextT IO ([MountainTerm], [Id]) -> MountainContextT IO ([MountainTerm], [Id])
      get_bound_ids id list_state  = do
        has_def <- hasDef id
        cur_state <- list_state
        let (cur_good, cur_bad) = cur_state
        if has_def
          then do
            def <- getDef id
            return (def:cur_good, cur_bad)
          else return (cur_good, id:cur_bad)

normalize :: MountainTerm -> MountainTerm
normalize l@(Literal _) = l
normalize Wildcard = Wildcard
normalize r@(Reference _) = r
normalize i@(Import (Import a)) = normalize $ Import a
normalize i@(Import a) = Import (normalize a)
normalize s@(Set xs) = Set (map normalize xs)
normalize e@(Tuple []) = e
normalize e@(Tuple [a]) = normalize a
normalize e@(Tuple (x:xs)) = do
  case normalize (Tuple xs) of
    Tuple [] -> normalize x
    Tuple new_xs -> Tuple (normalize x:new_xs)
    other -> Tuple [normalize x, other]
normalize e@(Function []) = e
normalize e@(Function [a]) = normalize a
normalize e@(Function (x:xs)) = do
  case normalize (Function xs) of
    Function new_xs -> Function (normalize x:new_xs)
    other -> Function [normalize x, other]
normalize e@(Either []) = e
normalize e@(Either [a]) = normalize a
normalize e@(Either (x:xs)) = do
  let x' = normalize x
  case x' of
    Either i -> normalize $ Either (i ++ xs)
    other ->
      case normalize (Either xs) of
        Either new_xs -> Either (x':new_xs)
        other -> Either [x', other]
normalize e@(Except []) = e
normalize e@(Except [a]) = normalize a
normalize e@(Except (x:xs)) = do
  let x' = normalize x
  case x' of
    Except i -> normalize $ Except (i ++ xs)
    other ->
      case normalize (Except xs) of
        Except new_xs -> Except (x':new_xs)
        other -> Except [x', other]
normalize e@(Each []) = e
normalize e@(Each [a]) = normalize a
normalize e@(Each (x:xs)) = do
  let x' = normalize x
  case x' of
    Each i -> normalize $ Each (i ++ xs)
    other ->
      case normalize (Each xs) of
        Each new_xs -> Each (x':new_xs)
        other -> Each [x', other]
normalize e@(Scope []) = e
normalize e@(Scope (x:xs)) = do
  let x' = normalize x
  case x' of
    Scope i -> normalize $ Scope (i ++ xs)
    other ->
      case normalize (Scope xs) of
        Scope new_xs -> Scope (x':new_xs)
        other -> Scope [x', other]
normalize c@(Call a b) = Call (normalize a) (normalize b)
normalize r@(Refine (Refine x y) b) = normalize $ Refine x (Each [y,b])
normalize r@(Refine a b) = Refine (normalize a) (normalize b)
normalize h@(Has a (Has x y)) = Has (Each [a,x]) y
normalize h@(Has a b) = Has (normalize a) (normalize b)
normalize b@(Bind x a@(Bind y z)) = 
  if x == y
    then normalize $ Bind x z
    else Bind (normalize x) (normalize a)
normalize b@(Bind x y) =
  if x == y
    then normalize y
    else Bind (normalize x) (normalize y)
normalize s@(Select s2@(Select _ _) id1) = do
  case normalize s2 of
    Select new_a ids -> Select new_a (ids ++ id1)
    other -> Select other id1
normalize r@(Recursive id (Reference n)) = 
  if id == n
    then Reference n
    else r
normalize r@(Recursive id (Recursive id2 x)) = 
  if id == id2
    then normalize $ Recursive id x
    else Recursive id $ normalize (Recursive id2 x)
normalize r@(Recursive id x) = Recursive id (normalize x)
normalize s@(Select a ids) = Select (normalize a) ids
normalize s@(Hide s2@(Hide _ _) id1) = do
  case normalize s2 of
    Hide new_a ids -> Hide new_a (id1 ++ ids)
    other -> Hide other id1
normalize s@(Hide a []) = normalize a
normalize s@(Hide a ids) = Hide (normalize a) ids
normalize u@(Unique Nil x) = normalize x
normalize u@(Unique h (Unique h2 x)) =
  if h == h2
    then Unique h (normalize x)
    else Unique h (Unique h2 (normalize x))
normalize u@(Unique h x) = Unique h (normalize x)
normalize c@(Context env c2@(Context env2 a)) = do
  case normalize c2 of
    Context new_env new_a -> Context (M.union new_env env) new_a
    other -> Context env other
normalize c@(Context env a) =
  if M.null env
    then normalize a
    else Context (M.map normalize env) (normalize a)

step :: MountainTerm -> MountainContextT IO MountainTerm
step t = normalize . fst . uniquify <$> step' t
  where
    step' :: MountainTerm -> MountainContextT IO MountainTerm
    step' l@(Literal _) = return l
    step' Wildcard = return Wildcard
    step' r@(Reference n) = getDef n `catchError` const (return r)
    step' (Import n) = do
      res <- step' n
      if res == n
        then stepImport n
        else return $ Import res
    step' (Set xs) = do
      res <- _stepSeqNoError xs
      return $ Set res
    step' (Either []) = throwError EmptyEither
    step' a@(Either xs) = do
      res <- Either <$> _stepEither xs
      if res /= a
        then return res
        else return $ _joinContexts Either xs
    step' d@(Except _) = throwError $ NotImplemented "Excepterence" d
    step' (Function xs) = do
      res <- _stepSeqNoError xs
      return $ Function res
    step' (Each []) = throwError EmptyEach
    step' (Each xs) = do
      res <- _stepSeqNoError xs
      return $ Each res
    step' (Tuple xs) = do
      res <- _stepSeqNoError xs
      return $ Tuple res
    step' (Scope inner) = do
      res <- _stepSeqNoError inner
      if res == inner
        then stepScope inner
        else return $ Scope res
    step' c@(Call x y) = do
      res <- _stepSeqNoError [x,y]
      let [x', y'] = res
      if x' == x && y' == y
        then stepCall x y
        else return $ Call x' y'
    step' r@(Refine x y) = do
      res <- _stepSeqNoError [x,y]
      let [x',y'] = res
      return $ Refine x' y'
    step' c@(Has x y) = do
      res <- _stepSeqNoError [x,y]
      let [x', y'] = res
      if x' == x && y' == y
        then has x y
        else return $ Has x' y'
    step' b@(Bind x y) = do
      res <- _stepSeqNoError [x,y]
      let [x',y'] = res
      return $ Bind x' y'
    step' r@(Recursive n x) = do
      -- we don't want it to unroll here, but in specific other places
      pushEnv (M.singleton n (Reference n))
      res <- _stepSeqNoError [x]
      popEnv
      let [x'] = res
      return $ Recursive n x'
    step' h@(Hide _ _) = throwError $ NotImplemented "hides" h
    step' (Select a ids) = do
      res <- step a
      if res == a
        then select a ids
        else return $ Select res ids
    -- step' (Hide a ids) = stepHide a ids
    step' (Unique h a) = Unique h <$> step a
    step' c@(Context env s) = do
      pushEnv env
      res <- step s
      newEnv <- popEnv
      if res == s && newEnv == env
        then return s
        else return (Context newEnv res)

_stepSeqNoError :: [MountainTerm] -> MountainContextT IO [MountainTerm]
_stepSeqNoError [] = return []
_stepSeqNoError (x:xs) = do
  res <- step x
  if res == x
    then (res :) <$> _stepSeqNoError xs
    else return (res:xs)

_stepEither :: [MountainTerm] -> MountainContextT IO [MountainTerm]
_stepEither [] = return []
_stepEither (c@(Context env x):xs) = do
  pushEnv env
  res <- step x
  newEnv <- popEnv
  if res == x && env == newEnv
    then do
      res <- _stepEither xs
      return $ c:res
    else return $ Context newEnv res:xs
  `catchError` (\e -> return xs)
_stepEither (x:xs) = _stepEither (Context M.empty x:xs)

_stepEach :: [MountainTerm] -> MountainContextT IO [MountainTerm]
_stepEach [] = return []
_stepEach (c@(Context env x):xs) = do
  pushEnv env
  res <- step x
  newEnv <- popEnv
  if res == x && env == newEnv
    then do
      res <- _stepEach xs
      return $ c:res
    else return $ Context newEnv res:xs
_stepEach (x:xs) = _stepEither (Context M.empty x:xs)

_joinContexts :: ([MountainTerm] -> MountainTerm) -> [MountainTerm] -> MountainTerm
_joinContexts joiner inner = do
  let (envs, terms) = collect inner
  let new_env = M.unionsWith (\a b -> joiner [a,b]) envs
  Context new_env (joiner terms)
  where
    collect :: [MountainTerm] -> ([Env MountainTerm], [MountainTerm])
    collect [] = ([],[])
    collect (Context e t:xs) = do
      let (es, ts) = collect xs
      (e:es, t:ts)
    collect (other:xs) = do
      let (es,ts) = collect xs
      (es, other:ts)

stepMany :: Int -> MountainTerm -> MountainContextT IO MountainTerm
stepMany 0 t = return t
stepMany i t = do
  res <- step t
  env <- getEnv
  tell [Step res env]
  if res == t
    then return res
    else stepMany (i-1) res

evaluate :: MountainTerm -> MountainContextT IO MountainTerm
evaluate t = do
  res <- step t
  env <- getEnv
  tell [Step res env]
  if res == t
    then return res
    else evaluate res

bind :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
bind a@(Context env z@(Bind x y)) b = return $ Bind (Context env x) (Bind (Context env y) b)
bind (Context env r@(Recursive id x)) r2@(Recursive id2 y) = return $ Bind (Context env (unroll r)) (Context (M.singleton id2 (Reference id)) y)
bind a@(Context env r@(Recursive id x)) b = throwError $ BadBind a b
bind (Context env r@(Reference n)) b@(Recursive x y) = do
  let res = M.lookup n env
  case res of
    Just r' -> return $ Bind r' b
    Nothing -> do
      putDef n b
      return b
bind a@(Context _ _) b@(Recursive _ _) = throwError $ BadBind a b
bind (Context env r@(Reference n)) b@(Bind x y) = do
  let res = M.lookup n env
  case res of
    Just r' -> return $ Bind r' b
    Nothing -> do
      res <- bind x y
      return $ Bind (Context env r) res
bind (Context env r@(Reference n)) b@(Reference m) = do
  let res = M.lookup n env
  case res of
    Just r' -> return $ Bind r' b
    Nothing ->
      if n == m
        then return (Reference m)
        else do
          putDef n b
          return b
bind c@(Context env x) b@(Reference n) = do
  res <- hasDef n
  if res
    then do
      def <- getDef n
      return $ Bind c def
    else do
      putDef n c
      return b
bind (Context env r@(Reference n)) b = do
  let res = M.lookup n env
  case res of
    Just r' -> return $ Bind r' b
    Nothing -> do
      putDef n b
      return b
bind a@(Context _ _) b@(Bind x y) = do
  res <- bind x y
  return $ Bind a res
bind a@(Context _ _) b@(Either inner) = return $ Either $ map (a `Bind`) inner
bind a@(Context _ _) b@(Each inner) = return $ Each $ map (a `Bind`) inner
bind (Context env a@(Except inner)) b = return $ Except $ map ((`Bind` b) . Context env) inner
bind a@(Context _ _) b@(Except inner) = return $ Except $ map (a `Bind`) inner
bind a@(Context _ _) b@(Context env x) = do
  pushEnv env
  res <- bind a x
  new_env <- popEnv
  case res of
    Bind a' b' -> return $ Bind a' (Context new_env b')
    other -> return $ Context new_env other
bind a@(Context _ _) (Import b) = return $ Import (Bind a b) -- TODO change to make imports bind afterwards
bind (Context env a@(Import _)) b = error "imports should be stepped before binding"
bind (Context env Wildcard) b = return b
bind (Context env a@(Literal x)) b@(Literal y) =
  if x == y
    then return a
    else throwError $ BadBind a b
bind (Context env a@(Literal _)) b = throwError $ BadBind a b
bind (Context env a@(Set [r@(Reference n)])) b@(Set elems2) = do
  let res = M.lookup n env
  case res of
    Just r' -> return $ Bind (Context env (Set [r'])) b
    Nothing -> return $ Bind r (Either elems2)
bind a@(Context env x@(Set elems)) b@(Set elems2) = return $ Bind (Context env (Either elems)) (Either elems2)
bind (Context env a@(Set elems)) b = throwError $ BadBind a b
bind (Context env a@(Function [])) b@(Function []) = return b
bind a@(Context env (Function _)) b@(Function (ib@(Bind ya yb):ys)) = do
  pushEnv M.empty
  res <- bind ya yb
  env' <- popEnv
  return $ Bind a (Context env' (Function (res:ys)))
bind a@(Context env (Function (x@(Bind xa Wildcard):xs))) b@(Function (y:ys)) = do
  pushEnv env
  res <- bind (Context env xa) y
  new_env <- popEnv
  case normalize res of
    b'@(Bind (Context env x') y') -> do -- TODO isRecursive should handle Contexts
      let final_env = M.union new_env env
      return $ Bind (Context final_env (Function (Bind x' Wildcard:xs))) (Function (y':ys))
    b'@(Bind x' y') ->
      return $ Bind (Context new_env (Function (Bind x' Wildcard:xs))) (Function (y':ys))
    other ->
      return $ Bind (Context new_env (Function (Wildcard:xs))) (Function (other:ys))
bind a@(Context env (Function (x@(Bind xa xb):xs))) b@(Function (y:ys)) = do
  res <- bind (Context env (Function (xb:xs))) (Function (y:ys))
  case normalize res of
    b'@(Bind (Context env (Function (x':xs'))) y') -> do
      return $ Bind (Context env (Function (Bind xa x':xs'))) y'
    b'@(Bind (Function (x':xs')) y') -> do
      return $ Bind (Function (Bind xa x':xs')) y'
    other -> return other
bind (Context env a@(Function (Wildcard:xs))) b@(Function (y:ys)) = do
  -- normalized here because Function [x] == x
  res <- bind (Context env (normalize $ Function xs)) (normalize $ Function ys)
  case normalize res of
    b'@(Bind (Context new_env xs') ys') -> -- TODO isRecursive should handle Contexts
      return $ Bind (Context new_env $ Function [Wildcard,xs']) (Function [y, ys'])
    b'@(Bind xs' ys') ->
      return $ Bind (Function [Wildcard,xs']) (Function [y, ys'])
    other -> return (Function [y,other])
bind (Context env a@(Function (x:xs))) b@(Function (y:ys)) = do
  res <- bind (Context env x) y
  case normalize res of
    b'@(Bind (Context env x') y') -> -- TODO isRecursive should handle Contexts
      return $ Bind (Function (x':xs)) (Function (y':ys))
    b'@(Bind x' y') ->
      return $ Bind (Function (Wildcard:xs)) (Function (y':ys))
    other -> return $ Bind (Function (Wildcard:xs)) (Function (other:ys))
bind (Context env a@(Function _)) b = throwError $ BadBind a b
bind (Context env a@(Tuple [])) b@(Tuple []) = return b
bind a@(Context env (Tuple _)) b@(Tuple (ib@(Bind ya yb):ys)) = do
  pushEnv M.empty
  res <- bind ya yb
  env' <- popEnv
  return $ Bind a (Context env' (Tuple (res:ys)))
bind a@(Context env (Tuple (x@(Bind xa Wildcard):xs))) b@(Tuple (y:ys)) = do
  pushEnv env
  res <- bind (Context env xa) y
  new_env <- popEnv
  case normalize res of
    b'@(Bind (Context env x') y') -> do -- TODO isRecursive should handle Contexts
      let final_env = M.union new_env env
      return $ Bind (Context final_env (Tuple (Bind x' Wildcard:xs))) (Tuple (y':ys))
    b'@(Bind x' y') ->
      return $ Bind (Context new_env (Tuple (Bind x' Wildcard:xs))) (Tuple (y':ys))
    other ->
      return $ Bind (Context new_env (Tuple (Wildcard:xs))) (Tuple (other:ys))
bind a@(Context env (Tuple (x@(Bind xa xb):xs))) b@(Tuple (y:ys)) = do
  res <- bind (Context env (Tuple (xb:xs))) (Tuple (y:ys))
  case normalize res of
    b'@(Bind (Context env (Tuple (x':xs'))) y') -> do
      return $ Bind (Context env (Tuple (Bind xa x':xs'))) y'
    b'@(Bind (Tuple (x':xs')) y') -> do
      return $ Bind (Tuple (Bind xa x':xs')) y'
    other -> return other
bind (Context env a@(Tuple (Wildcard:xs))) b@(Tuple (y:ys)) = do
  -- normalized here because Tuple [x] == x
  res <- bind (Context env (normalize $ Tuple xs)) (normalize $ Tuple ys)
  case normalize res of
    b'@(Bind (Context new_env xs') ys') -> -- TODO isRecursive should handle Contexts
      return $ Bind (Context new_env $ Tuple [Wildcard,xs']) (Tuple [y, ys'])
    b'@(Bind xs' ys') ->
      return $ Bind (Tuple [Wildcard,xs']) (Tuple [y, ys'])
    other -> return (Tuple [y,other])
bind (Context env a@(Tuple (x:xs))) b@(Tuple (y:ys)) = do
  res <- bind (Context env x) y
  case normalize res of
    b'@(Bind (Context env x') y') -> -- TODO isRecursive should handle Contexts
      return $ Bind (Tuple (x':xs)) (Tuple (y':ys))
    b'@(Bind x' y') ->
      return $ Bind (Tuple (Wildcard:xs)) (Tuple (y':ys))
    other -> return $ Bind (Tuple (Wildcard:xs)) (Tuple (other:ys))
bind (Context env a@(Tuple _)) b = throwError $ BadBind a b
bind (Context env a@(Each inner)) b = return $ Each $ map ((`Bind` b) . Context env) inner
bind (Context env a@(Either inner)) b = return $ Either $ map ((`Bind` b) . Context env) inner
bind (Context env a@(Scope _)) b = error "Scopes should be normalized away in binds"
bind (Context env a@(Call x y)) b@(Call x2 y2) = do
  resx <- bind x x2
  resy <- bind y y2
  return $ Call resx resy
bind (Context env (Call _ _)) _ = error "Calls should be normalized away in before bind"
bind a@(Context env (Refine _ _)) b = throwError $ NotImplemented "Refined binding" $ Bind a b
bind (Context env (Has typ x)) b = bind (Context env x) (Has (Context env typ) b)
bind a@(Context env (Select x ids)) b = throwError $ NotImplemented "binding on selects" $ Bind a b
bind a@(Context env (Hide x ids)) b = throwError $ NotImplemented "binding on hides" $ Bind a b
bind a@(Context env (Unique h x)) b@(Unique h2 y) = do
  if h /= h2
    then throwError $ BadBind a b
    else do
      res <- bind (Context env x) y
      case res of
        Bind (Context env a') b' -> return $ Bind (Context env (Unique h a')) (Unique h2 b')
        Bind a' b' -> return $ Bind (Unique h a') (Unique h2 b')
        other -> return $ Unique h2 other
bind a@(Context env (Unique h x)) other = throwError $ BadBind a other
bind (Context _ (Context _ _)) _ = error "nested contexts should have been normalized before binds"
bind a b = bind (Context M.empty a) b

has :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
    -- TODO: maybe a scope for the big has and small has so they don't overlap
-- right associative rules come first
has a@(Context _ _) b@(Has _ _) = error "has should be normalized before call"
has a@(Context env (Has x y)) b = do
  pushEnv env
  res <- has x y
  new_env <- popEnv
  case normalize res of
    h@(Has x' y') -> return $ Has (Has x' y') b
    y' -> return $ Has (Context new_env y')  b
has a@(Context _ _) b@(Either inner) = return $ Either $ map (a `Has`) inner
has a@(Context _ _) b@(Each inner) = return $ Each $ map (a `Has`) inner
has a@(Context _ _) b@(Except inner) = return $ Except $ map (a `Has`) inner
has a@(Context _ _) b@(Bind x y) = do
  res <- has a y
  case normalize res of
    b'@(Has a' y') -> -- TODO isRecursive should handle Contexts
      return $ Has a' (Bind x y')
    other -> return (Bind x other)
has t@(Context env (Reference n)) v@(Reference k) = do
  let nres = M.lookup n env
  case nres of
    Just def -> return $ Has def v
    Nothing -> do
      def <- getDef k
      return $ Has t def
  `catchError` const (return $ Has t v)
has t@(Context env (Reference n)) a = do
  let nres = M.lookup n env
  case nres of
    Just def -> return $ Has def a
    Nothing -> do
      let new_env = M.insert n (Set [a]) env
      return $ Has (Context new_env (Set [a])) a -- TODO MAKE THIS WILDCARD instead of Set [a]
has a@(Context _ (Set [])) b = throwError $ BadHas (normalize a) b
has (Context env (Set [x])) y@(Reference n) = do
  res <- hasDef n
  if res
    then Has (Context env (Set [x])) <$> getDef n
    else do
      putDef n x
      return x
has a@(Context env (Set [x])) b = return $ Scope [Bind (Context env x) b]
has a@(Context env (Set (x:xs))) b = return $ Either [Has (Context env (Set [x])) b, Has (Context env (Set xs)) b]
has a@(Context _ _) r@(Reference n) = do
  def <- getDef n
  return $ Has a def
  `catchError` const (return (Has a r))
has a@(Context env (Literal All)) b = return b
has a@(Context env Wildcard) b = return b
has a@(Context env (Literal (Thing _))) b = throwError $ BadHas (normalize a) b
has a@(Context env (Import x)) b = error "imports should have been stepped before executing has"
has (Context _ _) b@(Import x) = error "imports should have been stepped before executing has"
has a@(Context env (Bind x y)) b = return $ Has y b
has (Context env a@(Function [])) b@(Function []) = return b
has (Context env a@(Function (Bind xa Wildcard:xs))) b@(Function (Bind ya yb:ys)) = do
  pushEnv env
  res <- bind xa (Set [yb])
  new_env <- popEnv
  let unioned = M.union new_env env
  case normalize res of
    b'@(Bind x' y') -> return $ Has (Context unioned (Function (Bind x' Wildcard:xs))) (Function (Bind ya y':ys))
    other -> return $ Has (Context unioned (Function (Wildcard:xs))) b
has (Context env a@(Function (Bind xa Wildcard:xs))) b@(Function (y:ys)) = do
  pushEnv env
  res <- bind xa (Set [y])
  new_env <- popEnv
  let unioned = M.union new_env env
  case normalize res of
    b'@(Bind x' y') -> return $ Has (Context unioned (Function (Bind x' Wildcard:xs))) (Function (y':ys))
    other -> return $ Has (Context unioned (Function (Wildcard:xs))) b
has (Context env a@(Function (Bind xa xb:xs))) b@(Function (y:ys)) = do
  res <- has (Context env (Function (xb:xs))) b
  case normalize res of
    (Has (Context env' (Function (xb':xs'))) b') -> return $ Has (Context env' $ Function (Bind xa xb':xs')) b'
    (Has (Function (xb':xs')) b') -> return $ Has (Function (Bind xa xb':xs')) b'
    other -> return other
has (Context env a@(Function (Wildcard:xs))) b@(Function (y:ys)) = do
  -- normalized here because Function [x] == x
  pushEnv M.empty
  res <- has (Context env (normalize $ Function xs)) (normalize $ Function ys)
  main_env <- popEnv
  case normalize res of
    b'@(Has (Context env' xs') ys') -> return $ Has (Context env' $ Function [Wildcard,xs']) (Context main_env (Function [y,ys']))
    b'@(Has xs' ys') -> return $ Has (Function [Wildcard,xs']) (Context main_env (Function [y, ys']))
    other -> return (Context main_env (Function [y,other]))
has a@(Context env (Function (x:xs))) b@(Function (y:ys)) = do
  pushEnv M.empty
  res <- has (Context env x) y
  main_env <- popEnv
  if res /= Has (Context env x) y -- has was possible 
    then do
      case normalize res of
        b'@(Has (Context env' x') y') -> return $ Has (Context env' $ Function (x':xs)) (Context main_env (Function (y':ys)))
        b'@(Has x' y') -> return $ Has (Function (x':xs)) (Context main_env (Function (y':ys)))
        other -> return $ Has (Function (Wildcard:xs)) (Context main_env (Function (other:ys)))
    else do
      res <- has (Context env (Function (Wildcard:xs))) b
      case normalize res of
        (Has (Context env' (Function (Wildcard:xs'))) b') -> return $ Has (Context env' $ Function (x:xs')) b'
        (Has (Function (Wildcard:xs')) b') -> return $ Has (Function (x:xs')) b'
        (Has _ b') -> error "what is this case"
        other -> return $ Function [y,other]
has (Context env a@(Function _)) b = throwError $ BadHas a b
has (Context env a@(Tuple [])) b@(Tuple []) = return b
has (Context env a@(Tuple (Bind xa Wildcard:xs))) b@(Tuple (Bind ya yb:ys)) = do
  pushEnv env
  res <- bind xa (Set [yb])
  new_env <- popEnv
  let unioned = M.union new_env env
  case normalize res of
    b'@(Bind x' y') -> return $ Has (Context unioned (Tuple (Bind x' Wildcard:xs))) (Tuple (Bind ya y':ys))
    other -> return $ Has (Context unioned (Tuple (Wildcard:xs))) b
has (Context env a@(Tuple (Bind xa Wildcard:xs))) b@(Tuple (y:ys)) = do
  pushEnv env
  res <- bind xa (Set [y])
  new_env <- popEnv
  let unioned = M.union new_env env
  case normalize res of
    b'@(Bind x' y') -> return $ Has (Context unioned (Tuple (Bind x' Wildcard:xs))) (Tuple (y':ys))
    other -> return $ Has (Context unioned (Tuple (Wildcard:xs))) b
has (Context env a@(Tuple (Bind xa xb:xs))) b@(Tuple (y:ys)) = do
  res <- has (Context env (Tuple (xb:xs))) b
  case normalize res of
    (Has (Context env' (Tuple (xb':xs'))) b') -> return $ Has (Context env' $ Tuple (Bind xa xb':xs')) b'
    (Has (Tuple (xb':xs')) b') -> return $ Has (Tuple (Bind xa xb':xs')) b'
    other -> return other
has (Context env a@(Tuple (Wildcard:xs))) b@(Tuple (y:ys)) = do
  -- normalized here because Function [x] == x
  pushEnv M.empty
  res <- has (Context env (normalize $ Tuple xs)) (normalize $ Tuple ys)
  main_env <- popEnv
  case normalize res of
    b'@(Has (Context env' xs') ys') -> return $ Has (Context env' $ Tuple [Wildcard,xs']) (Context main_env (Tuple [y,ys']))
    b'@(Has xs' ys') -> return $ Has (Tuple [Wildcard,xs']) (Context main_env (Tuple [y, ys']))
    other -> return (Context main_env (Tuple [y,other]))
has a@(Context env (Tuple (x:xs))) b@(Tuple (y:ys)) = do
  pushEnv M.empty
  res <- has (Context env x) y
  main_env <- popEnv
  if res /= Has (Context env x) y -- has was possible 
    then do
      case normalize res of
        b'@(Has (Context env' x') y') -> return $ Has (Context env' $ Tuple (x':xs)) (Context main_env (Tuple (y':ys)))
        b'@(Has x' y') -> return $ Has (Tuple (x':xs)) (Context main_env (Tuple (y':ys)))
        other -> return $ Has (Tuple (Wildcard:xs)) (Context main_env (Tuple (other:ys)))
    else do
      res <- has (Context env (Tuple (Wildcard:xs))) b
      case normalize res of
        (Has (Context env' (Tuple (Wildcard:xs'))) b') -> return $ Has (Context env' $ Tuple (x:xs')) b'
        (Has (Tuple (Wildcard:xs')) b') -> return $ Has (Tuple (x:xs')) b'
        (Has _ b') -> error "what is this case"
        other -> return $ Tuple [y,other]
has (Context env a@(Tuple _)) b = throwError $ BadHas a b
has (Context env (Either xs)) b = return $ Either $ map ((`Has` b) . Context env) xs
has (Context env (Each xs)) b = return $ Each $ map ((`Has` b) . Context env) xs
has a@(Context env (Except inner)) b = return $ Except $ map ((`Has` b) . Context env) inner
has a@(Context env (Call _ _)) b = throwError $ BadHas (normalize a) b -- should be stepped
has a@(Context env (Refine _ _)) b = throwError $ NotImplemented "Refined Has" $ Has a b
has a@(Context env (Select x ids)) b = throwError $ BadHas (normalize a) b -- should be stepped
has a@(Context env (Hide x ids)) b = throwError $ BadHas (normalize a) b -- should be stepped
has a@(Context env (Unique h1 x)) b = do
  res <- has (Context env x) b
  case res of
    Has (Context env a') b' -> return $ Has (Context env (Unique h1 a')) b'
    Has a' b' -> return $ Bind (Unique h1 a') b'
    other -> return other
has (Context env a@(Scope _)) b = error "Scopes should be stepped away in has"
has (Context _ (Context _ _)) b = error "nested contexts should be normalized"
has a b = has (Context M.empty a) b

unroll :: MountainTerm -> MountainTerm
unroll (Recursive id x) = Context (M.singleton id x) x
unroll other = error "should be only used on recursive terms"

select :: MountainTerm -> [Id] -> MountainContextT IO MountainTerm
select l@(Literal (Thing id)) [x] =
  if id == x
    then return l
    else throwError $ BadSelect l [x]
select a@(Literal _) ids = throwError $ BadSelect a ids
select a@Wildcard ids = return $ Tuple $ map ((`Bind` Wildcard) . Reference) ids
select f@(Function _) ids = throwError $ BadSelect f ids
select s@(Scope _) ids = error "should be stepped before selecting"
select i@(Import _) ids = error "should be stepped before selecting"
select r@(Refine _ _) ids = throwError $ NotImplemented "refinements" (Select r ids)
select r@(Call _ _) ids = error "should be stepped before selecting"
select h@(Has _ _) ids = error "should be stepped before selecting"
select s@(Select _ _) ids = throwError $ NotNormalized $ Select s ids
select h@(Hide _ _) ids = throwError $ NotImplemented "Hides" $ Select h ids
select c@(Context env x) ids = do
  pushEnv env
  res <- select x ids
  new_env <- popEnv
  return $ Context env res
select b@(Bind (Reference n) x) ids = select (Tuple [b]) ids
select (Bind a y@(Bind b c)) ids = do
  res <- select y ids
  case res of
    Either [] -> select (Bind a c) ids
    something -> return something
select b@(Bind _ _) ids = return nothing
select r@(Recursive id x) ids = 
  if id `elem` ids
    then return r
    else return $ Select (unroll r) ids
select s@(Set xs) ids = return $ Either $ map (`Select` ids) xs
select s@(Either xs) ids = return $ Either $ map (`Select` ids) xs
select s@(Each xs) ids = return $ Each $ map (`Select` ids) xs
select s@(Except xs) ids = return $ Except $ map (`Select` ids) xs
select (Tuple _) [] = return unit
select s@(Tuple []) ids = throwError $ BadSelect s ids
select (Tuple (b@(Bind (Reference n) xb):xs)) ids = do
  let new_ids = filter (/= n) ids
  if n `elem` ids
    then do
      return $ Tuple [Bind (Reference n) xb,Select (Tuple xs) new_ids]
    else return $ Scope [b,Select (Tuple xs) new_ids]
select (Tuple (x@(Tuple (Literal (Thing n):_)):xs)) ids = do
  let new_ids = filter (/= n) ids
  if n `elem` ids
    then do
      return $ Tuple [x,Select (Tuple xs) new_ids]
    else return $ Scope [x,Select (Tuple xs) new_ids]
select (Tuple (x:xs)) ids = return $ Scope [x,Select (Tuple xs) ids]
select a@(Unique h x) ids = do
  res <- select x ids
  case res of
    (Select x' ids) -> return $ Select (Unique h x') ids
    other -> return $ Unique h other
select r@(Reference n) ids = return $ Select r ids

stepCall :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
stepCall a@(Function []) b = throwError $ BadCall a b
stepCall a@(Function (x:xs)) b = do
  pushEnv M.empty
  res <- bind x b
  new_env <- popEnv
  return $ Context new_env $ Function xs
  `catchError` (\case
    BadBind _ _ -> throwError $ BadCall a b
    other -> throwError other)
stepCall a@(Either []) b = throwError EmptyEither
stepCall a@(Either xs) b = return $ Either $ map (`Call` b) xs
stepCall a@(Each []) b = throwError $ BadCall a b
stepCall a@(Each xs) b = return $ Each $ map (`Call` b) xs
stepCall a b = return $ Call a b

stepScope :: [MountainTerm] -> MountainContextT IO MountainTerm
stepScope [] = throwError EmptyScope
stepScope (r@(Recursive id y):xs) =
  case xs of
    [] -> return r
    _ -> return $ Context (M.singleton id r) $ Scope xs
stepScope (b@(Bind x y):xs) = do
  pushEnv M.empty
  res <- bind x y
  new_env <- popEnv
  return $ Context new_env $ Scope (res : xs)
stepScope (x:xs) = do
  res <- step x
  if res == x
    then return $ case xs of
      [] -> res
      other -> Scope xs
    else return $ Scope (res:xs)

dotPathAsDir :: String -> FilePath
dotPathAsDir [] = []
dotPathAsDir ('.':xs) = '/':dotPathAsDir xs
dotPathAsDir (x:xs) = x:dotPathAsDir xs

setUnsetUniques :: MountainTerm -> IO MountainTerm
setUnsetUniques (Literal x) = return $ Literal x
setUnsetUniques Wildcard = return Wildcard
setUnsetUniques (Set ic) = Set <$> mapM setUnsetUniques ic
setUnsetUniques u@(Unique Unset (Reference _)) = return u
setUnsetUniques (Unique Unset ic) = do
  id_ <- randHash
  Unique id_ <$> setUnsetUniques ic
setUnsetUniques (Unique h ic) = Unique h <$> setUnsetUniques ic
setUnsetUniques (Function xs) = Function <$> mapM setUnsetUniques xs
setUnsetUniques (Each xs) = Each <$> mapM setUnsetUniques xs
setUnsetUniques (Tuple xs) = Tuple <$> mapM setUnsetUniques xs
setUnsetUniques (Either xs) = Either <$> mapM setUnsetUniques xs
setUnsetUniques (Except xs) = Either <$> mapM setUnsetUniques xs
setUnsetUniques (Refine b p) = Refine <$> setUnsetUniques b <*> setUnsetUniques p
setUnsetUniques (Call b a) = Call <$> setUnsetUniques b <*> setUnsetUniques a
setUnsetUniques (Import i_c) = Import <$> setUnsetUniques i_c
setUnsetUniques (Has b s) = Has <$> setUnsetUniques b <*> setUnsetUniques s
setUnsetUniques (Scope xs) = Scope <$> mapM setUnsetUniques xs
setUnsetUniques (Bind a b) = Bind <$> setUnsetUniques a <*> setUnsetUniques b
setUnsetUniques (Recursive id b) = Recursive id <$> setUnsetUniques b
setUnsetUniques (Reference id) = return $ Reference id
setUnsetUniques (Select a ids) = Select <$> setUnsetUniques a <*> return ids
setUnsetUniques (Hide a ids) = Hide <$> setUnsetUniques a <*> return ids
setUnsetUniques (Context env a) = Context <$> mapM setUnsetUniques env <*> setUnsetUniques a

uniquify :: MountainTerm -> (MountainTerm, Hash)
uniquify (Literal x) = (Literal x, Nil)
uniquify Wildcard = (Wildcard, Nil)
uniquify (Set ic) = do
  let (new_ic, hash) = unzip $ map uniquify ic
  let new_hash = sumHash hash
  (Unique new_hash (Set new_ic), new_hash)
uniquify (Unique Unset ic) = error "unset unique"
uniquify u@(Unique h (Literal _)) = (u,h)
uniquify u@(Unique h (Reference _)) = (u,h)
uniquify u@(Unique h (Tuple [])) = (u,h)
uniquify u@(Unique h x) = uniquify x
uniquify (Function xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let unduped = (map head . filter (\x -> length x == 1) . group . sort) hash
  let final_hash = seqHash $ filter (`elem` unduped) hash
  (Unique final_hash (Function new_ic), final_hash)
uniquify (Each xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let new_hash = sumHash hash
  (Unique new_hash (Each new_ic), new_hash)
uniquify (Tuple xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let new_hash = seqHash hash
  (Unique new_hash (Tuple new_ic), new_hash)
uniquify (Either xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let new_hash = sumHash hash
  (Unique new_hash (Either new_ic), new_hash)
uniquify (Except xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let new_hash = sumHash hash
  (Unique new_hash (Set new_ic), new_hash)
uniquify (Refine a b) = do
  let (a', hash_a) = uniquify a
  let (b', hash_b) = uniquify b
  (Refine (Unique hash_a a') (Unique hash_b b'), seqHash [hash_a, hash_b])
uniquify (Call a b) = do
  let (a', hash_a) = uniquify a
  let (b', hash_b) = uniquify b
  (Call (Unique hash_a a') (Unique hash_b b'), seqHash [hash_a, hash_b])
uniquify i@(Import i_c) = (i, Nil)
uniquify (Has a b) = do
  let (a', hash_a) = uniquify a
  let (b', hash_b) = uniquify b
  (Has (Unique hash_a a') (Unique hash_b b'), seqHash [hash_a, hash_b])
uniquify (Scope xs) = do
  let (new_ic, hash) = unzip $ map uniquify xs
  let new_hash = seqHash hash
  (Unique new_hash (Scope new_ic), new_hash)
uniquify (Bind a b) = do
  let (a', hash_a) = uniquify a
  let (b', hash_b) = uniquify b
  (Bind (Unique hash_a a') (Unique hash_b b'), seqHash [hash_a, hash_b])
uniquify (Recursive id b) = do
  let (b', hash_b) = uniquify b
  (Unique hash_b (Recursive id b'), hash_b)
uniquify (Reference id) = (Reference id, Nil)
uniquify (Select a ids) = do
  let (new_ic, hash) = uniquify a
  (Select new_ic ids, hash)
uniquify (Hide a ids) = do
  let (new_ic, hash) = uniquify a
  (Hide new_ic ids, hash)
uniquify (Context env a) = do
  let env_as_list = M.toList env
  let inner_foo :: (Id, Structure MountainLiteral) -> ([(Id, MountainTerm)], [Hash]) -> ([(Id, MountainTerm)], [Hash]) =
        \(id,term) (cur_list, cur_hashes) -> do {
            let (new_term, hash) = uniquify term
          ; ((id, new_term):cur_list, hash:cur_hashes)
        }
  let (new_map_list, hashes) :: ([(Id,MountainTerm)], [Hash]) = foldr inner_foo ([],[]) env_as_list
  let (a', hash) = uniquify a
  let final_hash = sumHash (hash:hashes)
  (Unique final_hash (Context (M.fromList new_map_list) a'), final_hash)

stepImport :: MountainTerm -> MountainContextT IO MountainTerm
stepImport r@(Reference n) = do
  opt <- getOptions
  let Options parser repo file_ext = opt
  let full_path = repo ++ dotPathAsDir n ++ file_ext
  file_exist <- lift $ doesFileExist full_path
  if not file_exist
    then throwError $ BadImport r
    else do
      res <- lift $ parser full_path
      lift $ fst . uniquify <$> setUnsetUniques res
stepImport (Bind n@(Reference _) i) = do
  res <- stepImport i
  return $ case res of
    Bind _ b -> Bind n b
    Import x -> Import (Bind n x)
    other -> Bind n other
stepImport s@(Select r@(Reference n) l@(id:ids)) = do
  opt <- getOptions
  let Options _ repo file_ext = opt
  let dir_path = repo ++ dotPathAsDir n
  dirExist <- lift $ doesDirectoryExist dir_path
  fileExist <- lift $ doesFileExist (dir_path ++ file_ext)
  if dirExist
    then
      case ids of
        [] -> return $ Import $ Reference (n ++ "." ++ id)
        something -> return $ Import $ Select (Reference (n ++ "." ++ id)) ids
  else if fileExist
    then return $ Select (Import r) l
  else throwError $ BadImport s
stepImport i@(Import _) = throwError $ NotNormalized i
stepImport something = error $ "what is this import case? " ++ show something


validate :: MountainTerm -> Bool
validate _ = True