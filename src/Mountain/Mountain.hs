{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mountain.Mountain where

import Mountain.Hash
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
import qualified Data.Set as S
import Data.Either
import Data.Maybe
import Debug.Trace (trace)


type Id = [Char]

type Env a = M.Map Id a

data Structure a =
    Literal a
  | Wildcard
  | Reference Id
  | Import (Structure a)
  | Set [Structure a] -- TODO MAKE THIS AN ACTUAL SET
  | Tuple [Structure a]
  | Function [Structure a]
  | Either [Structure a]
  | Each [Structure a]
  | Scope [Structure a]
  | Unique Hash (Structure a)
  | Call (Structure a) (Structure a)
  | Has (Structure a) (Structure a)
  | Bind (Structure a) (Structure a)
  | Refine (Structure a) (Structure a)
  | Select (Structure a) [Id]
  | Context (Env (Structure a)) (Structure a)
  -- | Hide (Structure a) [Id] TODO ADD HIDE BACK
  deriving (Eq, Show)

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
  fmap f (Scope x) = Scope $ map (fmap f) x
  fmap f (Call x y) = Call (fmap f x) (fmap f y)
  fmap f (Refine x y) = Refine (fmap f x) (fmap f y)
  fmap f (Has x y) = Has (fmap f x) (fmap f y)
  fmap f (Bind x y) = Bind (fmap f x) (fmap f y)
  fmap f (Select x ids) = Select (fmap f x) ids
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
        (Refine b p) -> Refine (recurse b) (recurse p)
        (Call b a) -> Call (recurse b) (recurse a)
        (Import i_c) -> Import (recurse i_c)
        (Has b s) -> Has (recurse b) (recurse s)
        (Scope xs) -> Scope $ map recurse xs
        (Bind a b) -> Bind (recurse a) (recurse b)
        (Reference id) -> Reference id
        (Select a ids) -> Select (recurse a) ids
        -- (Hide a ids) -> Hide (recurse a) ids
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
        (Refine x y) -> f [b,recurse x,recurse y]
        (Call x y) -> f [b,recurse x,recurse y]
        (Import i_c) -> f [b, recurse i_c]
        (Has x y) -> f [b,recurse x,recurse y]
        (Scope xs) -> f (b:map recurse xs)
        (Bind x y) -> f [b,recurse x,recurse y]
        (Reference id) -> b
        (Select a ids) -> f [b,recurse a]
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
  hash (Call a b) = seqHash [hashStr "Call", hash a, hash b]
  hash (Refine a b) = seqHash [hashStr "Refine", hash a, hash b]
  hash (Has a b) = seqHash [hashStr "Has", hash a, hash b]
  hash (Bind a b) = seqHash [hashStr "Bind", hash a, hash b]
  hash (Select a ids) = seqHash [hashStr "Select", hash a, seqHash $ map hashStr ids]
  -- hash (Hide a ids) = seqHash [hashStr "Hide", hash a, seqHash $ map hashStr ids]
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
  | All
  deriving (Eq, Show)

instance Hashable MountainLiteral where
  hash (Thing name) = hashStr name
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
    environment :: [Env MountainTerm],
    var_name_counter :: Int
  } deriving (Eq, Show)

toList :: MountainEnv -> [[(Id, MountainTerm)]]
toList (MountainEnv _ env _) = map M.toList env

dummyEnv :: MountainEnv
dummyEnv = MountainEnv (Options (const $ return empty) "" "") [] 0

data MountainError =
  UnknownError |
  NotImplemented MountainTerm |
  NotNormalized MountainTerm |
  EmptyEither |
  EmptyEach |
  BadBind MountainTerm MountainTerm |
  BindingDiffUniques MountainTerm MountainTerm |
  UnboundReference MountainTerm |
  UnboundId Id |
  PopEmptyEnvStack |
  FileNotExist MountainTerm |
  DirNotExist MountainTerm |
  NestedImport MountainTerm |
  EmptyScope |
  BadCall MountainTerm MountainTerm |
  BadHas MountainTerm MountainTerm |
  BadSelect MountainTerm [Id] |
  BadImport MountainTerm |
  EmptySelect MountainTerm |
  InvalidTerm MountainTerm |
  BadType MountainTerm |
  NeedEvaluationBeforeCall MountainTerm
  deriving (Show, Eq)

data MountainLog =
  -- ResolvedRef MountainTerm MountainTerm |
  Step MountainTerm MountainEnv
  | UnknownLog
  deriving (Show, Eq)

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
  let MountainEnv a b c = env
  put $ MountainEnv a (new_env : b) c

popEnv :: (Monad m) => MountainContextT m (Env MountainTerm)
popEnv = do
  (env :: MountainEnv) <- get
  let MountainEnv a b c = env
  case b of
    [] -> return M.empty
    (x:xs) -> do
      put $ MountainEnv a xs c
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
  let MountainEnv a b c = env
  case b of
    [] -> throwError $ UnboundId id
    (x:_) -> do
      case M.lookup id x of
        Nothing -> do
          popEnv
          res <- getDef id
          pushEnv x
          return res
        Just val -> return val

getFreshRef :: (Monad m) => MountainContextT m MountainTerm
getFreshRef = do
  (env :: MountainEnv) <- get
  let MountainEnv a b c = env
  let new_name = "v" ++ show c
  put $ MountainEnv a b (c + 1)
  return $ Reference new_name

putDef :: (Monad m) => Id -> MountainTerm -> MountainContextT m ()
putDef id v = do
  (env :: MountainEnv) <- get
  let MountainEnv a b c = env
  case b of
    [] -> put $ MountainEnv a [M.singleton id v] c
    (m:ms) -> put $ MountainEnv a (M.insert id v m:ms) c

unionEnv :: (Monad m) => Env MountainTerm -> MountainContextT m ()
unionEnv new_env = do
  (env :: MountainEnv) <- get
  let MountainEnv a b c = env
  case b of
    [] -> put $ MountainEnv a [new_env] c
    (m:ms) -> put $ MountainEnv a (M.union new_env m:ms) c

getOptions :: (Monad m) => MountainContextT m MountainOptions
getOptions = do
  (env :: MountainEnv) <- get
  let MountainEnv opt _ _ = env
  return opt

getEnv :: (Monad m) => MountainContextT m ([Env MountainTerm])
getEnv = do
  (env :: MountainEnv) <- get
  let MountainEnv _ e _ = env
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
normalize s@(Set _) = s
normalize e@(Tuple []) = e
normalize e@(Tuple [a]) = normalize a
normalize e@(Tuple (x:xs)) = do
  case normalize (Tuple xs) of
    Tuple [] -> Tuple (normalize x:[Tuple []])
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
normalize e@(Either (a@(Either x):b)) = do
  normalize $ case normalize a of
    Either new_a -> Either $ new_a ++ b
    other -> Either (other:b)
normalize e@(Either (x:xs)) = do
  case normalize (Either xs) of
    Either new_xs -> Either (normalize x:new_xs)
    other -> Either [normalize x, other]
normalize e@(Each []) = e
normalize e@(Each [a]) = normalize a
normalize e@(Each (a@(Each x):b)) = do
  normalize $ case normalize a of
    Each new_a -> Each $ new_a ++ b
    other -> Each (other:b)
normalize e@(Each (x:xs)) = do
  case normalize (Each xs) of
    Each new_xs -> Each (normalize x:new_xs)
    other -> Each [normalize x, other]
normalize e@(Scope []) = e
normalize e@(Scope [a]) = normalize a
normalize e@(Scope (x:xs)) = do
  case normalize (Scope xs) of
    Scope new_xs -> Scope (normalize x:new_xs)
    other -> Scope [normalize x, other]
normalize c@(Call a b) = Call (normalize a) (normalize b)
normalize r@(Refine (Refine x y) b) = normalize $ Refine x (Each [y,b])
normalize r@(Refine a b) = Refine (normalize a) (normalize b)
normalize h@(Has a (Has x y)) = Has (Each [a,x]) y
normalize h@(Has a b) = Has (normalize a) (normalize b)
normalize b@(Bind x y) = Bind (normalize x) (normalize y)
normalize s@(Select s2@(Select _ _) id1) = do
  case normalize s2 of
    Select new_a ids -> Select new_a (id1 ++ ids)
    other -> Select other id1
normalize s@(Select a []) = normalize a
normalize s@(Select a ids) = Select (normalize a) ids
-- normalize s@(Hide s2@(Hide _ _) id1) = do
--   case normalize s2 of
--     Hide new_a ids -> Hide new_a (id1 ++ ids)
--     other -> Hide other id1
-- normalize s@(Hide a []) = normalize a
-- normalize s@(Hide a ids) = Hide (normalize a) ids
normalize u@(Unique h x) = Unique h (normalize x)
normalize c@(Context env c2@(Context env2 a)) = do
  case normalize c2 of
    Context new_env new_a -> Context (M.union new_env env) new_a
    other -> Context env other
normalize c@(Context env a) =
  if M.null env
    then normalize a
    else Context (M.map normalize env) (normalize a)

chain :: MountainTerm -> MountainContextT IO MountainTerm
chain (Context env t) = do
  pushEnv env
  res <- chain t
  new_env <- popEnv
  return $ Context new_env t
chain f@(Function (x:xs)) = do
  res <- evaluate x >>= stepBind
  return $ Function xs
chain f@(Tuple (x:xs)) = do
  res <- evaluate x >>= stepBind
  return $ Tuple xs
chain other = return other


stepBind :: MountainTerm -> MountainContextT IO MountainTerm
stepBind b@(Bind x y) = normalize <$> bind x y
stepBind other = step other

step :: MountainTerm -> MountainContextT IO MountainTerm
step t = normalize <$> step' t
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
    step' (Either xs) = Either <$> _stepSeqFilterError xs
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
        then stepHas x y
        else return $ Has x' y'
    step' b@(Bind x y) = do
      if isRecursive b
        then do
          let Reference n = x
          pushEnv (M.singleton n x) 
          res <- _stepSeqNoError [x,y]
          popEnv
          let [x',y'] = res
          return $ Bind x' y'
        else do
          res <- _stepSeqNoError [x,y]
          let [x',y'] = res
          return $ Bind x' y'
    step' (Select a ids) = do
      res <- step a
      if res == a
        then stepSelect a ids
        else return $ Select res ids
    -- step' (Hide a ids) = stepHide a ids
    step' (Unique h a) = Unique h <$> step a
    step' c@(Context env s) = do
      pushEnv env
      res <- step s
      newEnv <- popEnv
      if res == s
        then return s
        else return (Context newEnv res)

_stepSeqNoError :: [MountainTerm] -> MountainContextT IO [MountainTerm]
_stepSeqNoError [] = return []
_stepSeqNoError (x:xs) = do
  res <- step x
  if res == x
    then (res :) <$> _stepSeqNoError xs
    else return (res:xs)

_stepSeqFilterError :: [MountainTerm] -> MountainContextT IO [MountainTerm]
_stepSeqFilterError [] = return []
_stepSeqFilterError (x:xs) = do
  res <- step x
  if res == x
    then (res :) <$> _stepSeqFilterError xs
    else return (res:xs)
  `catchError` (\e -> return xs)

debugTell :: (Show w, MonadWriter w m) => w -> m ()
-- debugTell x = trace (show x) (tell x)
debugTell x = tell x

stepMany :: Int -> MountainTerm -> MountainContextT IO MountainTerm
stepMany 0 t = return t
stepMany i t = do
  res <- step t
  env <- get
  debugTell [Step res env]
  if res == t
    then return res
    else stepMany (i-1) res

stepBindMany :: Int -> MountainTerm -> MountainContextT IO MountainTerm
stepBindMany 0 t = return t
stepBindMany i t = do
  res <- stepBind t
  env <- get
  debugTell [Step res env]
  if res == t
    then return res
    else stepBindMany (i-1) res

evaluate :: MountainTerm -> MountainContextT IO MountainTerm
evaluate t = do
  res <- step t
  env <- get
  debugTell [Step res env]
  if res == t
    then return res
    else evaluate res

-- Terms must be normalized
bind :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
bind a@(Bind x y) b = return $ Bind x (Bind y b)
bind a b@(Bind x y) = do
  if isRecursive b
    then case a of
      Reference n -> do
        putDef n b
        return b
      other -> do
        pushEnv M.empty
        res <- stepBind b
        new_env <- popEnv
        return $ Bind a (Context new_env res)
    else do
      res <- bind x y
      return $ Bind a res
bind a b@(Context _ _) = Bind a <$> step b
bind r@(Reference n) b =
  if isRecursive $ Bind r b
    then do
      putDef n (Bind r b)
      return b
    else do
      putDef n b
      return b
bind a (Import b) = return $ Import (Bind a b) -- TODO change to make imports bind afterwards
bind a@(Import _) b = throwError $ NeedEvaluationBeforeCall (Bind a b)
bind Wildcard b = return b
bind a@(Literal x) b@(Literal y) =
  if x == y
    then return a
    else throwError $ BadBind a b
bind a@(Literal _) b = throwError $ BadBind a b
bind a@(Set [r@(Reference n)]) b@(Set elems2) =
  return $ Bind r (Either elems2)
bind a@(Set elems) b@(Set elems2) =
  if elems == elems2
    then return b
    else throwError $ BadBind a b
bind a@(Set elems) b = throwError $ BadBind a b
bind a@(Function []) b@(Function []) = return b
bind a@(Function (x:xs)) b@(Function (y:ys)) = do
  pushEnv M.empty
  res <- normalize <$> stepBind y
  newEnv <- popEnv
  if res /= y
    then return $ Bind a (Context newEnv (Function (res:ys)))
    else case x of
      Wildcard -> do
        res <- bind (Function xs) (Function ys)
        case res of
          Bind a _ -> return $ Bind a b
          Function _ -> return b
          other -> error "what is this case?"
      other -> do
        _ <- bind x y
        return $ Bind (Function (Wildcard:xs)) b
bind a@(Function _) b = throwError $ BadBind a b
bind a@(Tuple []) b@(Tuple []) = return b
bind a@(Tuple (x:xs)) b@(Tuple (y:ys)) = do
  pushEnv M.empty
  res <- normalize <$> stepBind y
  newEnv <- popEnv
  if res /= y
    then return $ Bind a (Context newEnv (Tuple (res:ys)))
    else case x of
      Wildcard -> do
        res <- bind (Tuple xs) (Tuple ys)
        case res of
          Bind a _ -> return $ Bind a b
          Tuple _ -> return b
          other -> error "what is this case?"
      other -> do
        _ <- bind x y
        return $ Bind (Tuple (Wildcard:xs)) b
bind a@(Tuple _) b = throwError $ BadBind a b
bind a@(Either inner) b = return $ Either $ map (`Bind` b) inner
bind a@(Each inner) b = return $ Each $ map (`Bind` b) inner
bind a@(Scope _) b = throwError $ BadBind a b
bind a@(Call x y) b@(Call x2 y2) = do
  resx <- bind x x2
  resy <- bind y y2
  return $ Call resx resy
bind a@(Call _ _) b = throwError $ BadBind a b
bind a@(Refine x y) b@(Refine x2 y2) = do
  resx <- bind x x2
  resy <- bind y y2
  return $ Call resx resy
bind a@(Refine _ _) b = throwError $ BadBind a b
bind a@(Has typ x) b = bind x (Has typ b)
bind a@(Select x ids) b = throwError $ BadBind a b
-- bind a@(Hide x ids) b = throwError $ BindHide a b
-- bind a b@(Hide y ids) = throwError $ BindHide a b
bind a@(Unique h c) b@(Unique h2 c2) = do
  res <- bind c c2
  if h == h2
    then return $ Unique h2 res
    else throwError $ BadBind a b
bind a@(Unique _ _) b = throwError $ BadBind a b
bind a@(Context _ _) b = throwError $ BadBind a b

getBindings :: Structure MountainLiteral -> MountainContextT IO (Env (Structure MountainLiteral))
getBindings = error "not implemented"

stepHas :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
stepHas x y = do
  has x y
  `catchError` (\case
      BadHas _ _ -> do
        step_x <- step x
        step_y <- step y
        if step_x == x && step_y == y
          then throwError $ BadHas x y
          else return $ Has step_x step_y
      other -> throwError other)

has :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
    -- TODO: maybe a scope for the big has and small has so they don't overlap
-- right associative rules come first
has a (Either ys) = return $ Either (map (Has a) ys)
has a (Each ys) = return $ Each (map (Has a) ys)
has t@(Reference n) v@(Reference k) = do
  def <- getDef n
  return $ Has def v
  `catchError` (\e -> do {
      def <- getDef k
    ; return $ Has t def
    } `catchError` const (return $ Has t v))
has t@(Reference n) a = do
  def <- getDef n
  return $ Has def a
  `catchError` (\e -> do
    putDef n (Set [a])
    return a)
has (Set [x]) y@(Reference n) = do
  putDef n x
  return x
has a r@(Reference n) = do
  def <- getDef n
  return $ Has a def
  `catchError` const (return (Has a r))
-- left associative rules
has a@(Literal All) b = return b
has Wildcard b = return b
has a@(Literal (Thing _)) b = throwError $ BadHas a b
has a@(Import x) b = throwError $ BadHas a b
has a b@(Import x) = throwError $ BadHas a b
has a@(Set []) b = throwError $ BadHas a b
has a@(Set [x]) b = do
  pushEnv M.empty
  bind x b
  popEnv
  return b
  `catchError` (\case
    BadBind _ _ -> throwError (BadHas a b)
    other -> throwError other
  )
has (Set (x:xs)) b = return $ Either [Has (Set [x]) b, Has (Set xs) b]
has (Tuple []) (Tuple []) = return $ Tuple []
has (Tuple [x]) (Tuple [y]) = return $ Tuple [Has x y]
has (Tuple (x:xs)) (Tuple (y:ys)) = return $ Tuple [Has x y, Has (Tuple xs) (Tuple ys)]
has a@(Tuple _) b = throwError $ BadHas a b
has (Either xs) b = return $ Either $ map (`Has` b) xs
has (Function []) (Function []) = return $ Function []
has (Function [x]) (Function [y]) = return $ Function [Has x y]
has (Function (x:xs)) (Function (y:ys)) = return $ Function [Has x y,Has (Function xs) (Function ys)]
has a@(Function _) b = throwError $ BadHas a b
has (Each xs) b = return $ Each $ map (`Has` b) xs
has a@(Scope _) b = throwError $ BadHas a b -- should be stepped
has a@(Call _ _) b = throwError $ BadHas a b -- should be stepped
has a@(Refine _ _) b = throwError $ NotImplemented $ Has a b
has a@(Has _ _) b = throwError $ NotNormalized $ Has a b
has a@(Bind x y) b = throwError $ BadHas a b -- should be stepped
has a@(Select x ids) b = throwError $ BadHas a b -- should be stepped
has a@(Unique h1 x) b@(Unique h2 y) =
  if h1 == h2
    then return $ Unique h1 $ Has x y
    else throwError $ BadHas a b
has a@(Unique _ _) b = throwError $ BadHas a b
has a@(Context _ _) b = throwError $ BadHas a b

stepSelect :: MountainTerm -> [Id] -> MountainContextT IO MountainTerm
stepSelect a ids = do
  select a ids
  `catchError` (\case
    b@(BadSelect x idx) -> do
      stepa <- step a
      if stepa == a
        then throwError b
        else return $ Select stepa ids
    other -> throwError other
  )

select :: MountainTerm -> [Id] -> MountainContextT IO MountainTerm
select something [] = return something
select l@(Literal (Thing id)) [x] =
  if id == x
    then return l
    else throwError $ BadSelect l [x]
select s@(Set xs) ids = select (Either xs) ids
select s@(Tuple []) ids = throwError $ BadSelect s ids
select (Tuple (x@(Bind a b):xs)) ids = do -- TODO fix leaking scope
  bind a b
  res <- getBoundIds ids
  let (found_vals, rest_ids) = res
  return $ Tuple (found_vals ++ [Select (Tuple xs) rest_ids])
select (Tuple (x:xs)) ids = return $ Select (Tuple xs) ids
select s@(Either []) ids = throwError $ BadSelect s ids
select s@(Either ((Bind a b):xs)) ids = do
  pushEnv M.empty
  bind a b
  res <- getBoundIds ids
  let (found_vals, res_ids) = res
  popEnv
  return $ Tuple $ found_vals ++ [Select (Either xs) res_ids]
select (Either (x:xs)) ids = return $ Select (Either xs) ids
select (Unique h xs) ids = Unique h <$> select xs ids
select (Reference n) b = do
  d <- getDef n
  return $ Select d b
select a b = throwError $ BadSelect a b

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
stepCall a b = error "bad evaluate call"

stepScope :: [MountainTerm] -> MountainContextT IO MountainTerm
stepScope [] = throwError EmptyScope
stepScope [x] = return x
stepScope (x:xs) = do
  res <- stepBind x
  if res == x
    then return $ Scope xs
    else return $ Scope (res:xs)

dotPathAsDir :: String -> FilePath
dotPathAsDir [] = []
dotPathAsDir ('.':xs) = '/':dotPathAsDir xs
dotPathAsDir (x:xs) = x:dotPathAsDir xs

stepImport :: MountainTerm -> MountainContextT IO MountainTerm
stepImport r@(Reference n) = do
  opt <- getOptions
  let Options parser repo file_ext = opt
  let full_path = repo ++ dotPathAsDir n ++ file_ext
  file_exist <- lift $ doesFileExist full_path
  if not file_exist
    then throwError $ BadImport r
    else lift $ parser full_path
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
    then return $ Import $ Select (Reference (n ++ "." ++ id)) ids
  else if fileExist
    then return $ Select (Import r) l
  else throwError $ BadImport s
stepImport i@(Import _) = throwError $ NotNormalized i
stepImport something = error $ "what is this import case? " ++ show something


validate :: MountainTerm -> Bool
validate _ = True
-- LHS of Bindings must be structures of references.
-- LHS of Bindings must not contain sub bindings
-- Structural bindings are not allowed within tuples and functions