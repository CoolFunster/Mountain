{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mountain.Mountain where

import Mountain.Hash
import qualified Data.Map as M
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


type Id = [Char]

type Env a = M.Map Id a

data Structure a =
    Literal a
  | Reference Id
  | Import (Structure a)
  | Set [Structure a]
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

data TransformResult a =
  Result a |
  Recurse a
  deriving (Eq, Show)

transform :: (Structure a -> TransformResult (Structure a)) -> Structure a -> Structure a
transform transformer input_structure = do
  case transformer input_structure of
    Result s -> s
    Recurse s -> do
      let recurse = transform transformer
      case s of
        (Literal x) -> Literal x
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

check' :: ([Bool] -> Bool) -> (Structure a -> Bool) -> Structure a -> Bool
check' aggregator checker l@(Literal _) = checker l
check' aggregator checker r@(Reference _) = checker r
check' aggregator checker u@(Unique h ic) = aggregator [checker u, check' aggregator checker ic]
check' aggregator checker i@(Import ic) = aggregator [checker i,  check' aggregator checker ic]
check' aggregator checker s@(Set xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Function xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Each xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Tuple xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Either xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Scope xs) = aggregator $ checker s : map (check' aggregator checker) xs
check' aggregator checker s@(Call a b) = aggregator [checker s, check' aggregator checker a, check' aggregator checker b]
check' aggregator checker s@(Refine a b) = aggregator [checker s, check' aggregator checker a, check' aggregator checker b]
check' aggregator checker s@(Has a b) = aggregator [checker s, check' aggregator checker a, check' aggregator checker b]
check' aggregator checker s@(Bind a b) = aggregator [checker s, check' aggregator checker a, check' aggregator checker b]
check' aggregator checker s@(Select a ids) = aggregator [checker s, check' aggregator checker a]
-- check' aggregator checker s@(Hide a ids) = aggregator [checker s, checker a]
check' aggregator checker s@(Context env a) = do
  let env_vals = M.foldr (:) [] env
  aggregator $ checker s : check' aggregator checker a : map (check' aggregator checker) env_vals

checkAll :: (Structure a -> Bool) -> Structure a -> Bool
checkAll = check' and

checkAny :: (Structure a -> Bool) -> Structure a -> Bool
checkAny = check' or

replace :: (Eq a) => Structure a -> Structure a -> Structure a -> Structure a
replace old new input_s =
  let
    replaceOldWithNew :: (Eq a) => Structure a -> Structure a -> TransformResult (Structure a)
    replaceOldWithNew old_c new_c =
      if new_c == old_c
        then Result new_c
        else Recurse old_c
  in
    transform (replaceOldWithNew old) input_s

-- ############################# --
-- Basic Structure functionality --
-- ############################# --

instance (Show a, Hashable a) => Hashable (Structure a) where
  hash (Literal x)= hash x
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

isRecursive :: Structure a -> Bool
isRecursive (Bind (Reference n) s) =
  let
    hasRefOfn :: Structure a -> Bool
    hasRefOfn (Reference n) = True
    hasRefOfn _ = False
  in
    checkAny hasRefOfn s
isRecursive _ = False

-- ############################# --
--    Mountain  Stuff            --
-- ############################# --

-- hashing 
-- TODO: ensure "Any, ?" are reserved keywords in parser
data MountainLiteral =
    Thing String
  | All
  | Wildcard
  | Star
  deriving (Eq, Show)

instance Hashable MountainLiteral where
  hash (Thing name) = hashStr name
  hash All = hashStr "All"
  hash Wildcard = hashStr "?"
  hash Star = hashStr "*"

type MountainTerm = Structure MountainLiteral
newtype MountainEnv = MountainEnv (MountainImporter, [Env MountainTerm])

toList :: MountainEnv -> [[(Id, MountainTerm)]]
toList (MountainEnv (_, env)) = map M.toList env

instance Show MountainEnv where
  show (MountainEnv (_,e)) = show (map M.toList e)

instance Eq MountainEnv where
  (==) (MountainEnv (_,a)) (MountainEnv (_,b)) = a == b

dummyEnv :: MountainEnv
dummyEnv = MountainEnv (dummyImporter, [])

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
  BadCall MountainTerm |
  BadHas MountainTerm MountainTerm |
  BadSelect MountainTerm [Id] |
  EmptySelect MountainTerm |
  InvalidTerm MountainTerm
  deriving (Show, Eq)

data MountainLog =
  -- ResolvedRef MountainTerm MountainTerm |
  UnknownLog
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
pushEnv env = do
  (env :: MountainEnv) <- get
  let MountainEnv (imp, rmap) = env
  put $ MountainEnv (imp, M.empty : rmap)

popEnv :: (Monad m) => MountainContextT m (Env MountainTerm)
popEnv = do
  (env :: MountainEnv) <- get
  let MountainEnv (imp, rmap) = env
  case rmap of
    [] -> throwError PopEmptyEnvStack
    (x:xs) -> do
      put $ MountainEnv (imp, xs)
      return x

hasDef :: (Monad m) => Id -> MountainContextT m Bool
hasDef id = do
  getDef id
  return True
  `catchError` (\case
    UnboundId id -> return False
    other -> throwError other
  )

getDef :: (Monad m) => Id -> MountainContextT m MountainTerm
getDef id = do
  (env :: MountainEnv) <- get
  let MountainEnv (imp, rmap) = env
  case rmap of
    [] -> throwError $ UnboundId id
    (x:_) -> do
      case M.lookup id x of
        Nothing -> do
          popEnv
          res <- getDef id
          pushEnv x
          return res
        Just val -> return val

putDef :: (Monad m) => Id -> MountainTerm -> MountainContextT m ()
putDef id v = do
  (env :: MountainEnv) <- get
  let MountainEnv (imp, rmap) = env
  case rmap of
    [] -> put $ MountainEnv (imp, [M.singleton id v])
    (m:ms) -> put $ MountainEnv (imp, M.insert id v m:ms)

getImporter :: (Monad m) => MountainContextT m MountainImporter
getImporter = do
  (env :: MountainEnv) <- get
  let MountainEnv (imp, rmap) = env
  return imp

getEnv :: (Monad m) => MountainContextT m MountainEnv
getEnv = get

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
normalize c@(Context env a) = Context (M.map normalize env) (normalize a)

stepRec :: MountainTerm -> MountainContextT IO MountainTerm
stepRec b@(Bind x y) = bind x y
stepRec other = step other

step :: MountainTerm -> MountainContextT IO MountainTerm
step t = normalize <$> step' t
  where
    step' :: MountainTerm -> MountainContextT IO MountainTerm
    step' l@(Literal _) = return l
    step' (Reference n) = getDef n
    step' i@(Import _) = do
      importer <- getImporter
      evaluateImport importer i
    step' (Set xs) = Set <$> _stepSeqNoError xs
    step' (Either []) = throwError EmptyEither
    step' (Either xs) = Either <$> _stepSeqFilterError xs
    step' (Function xs) = Function <$> _stepSeqNoError xs
    step' (Each []) = throwError EmptyEach
    step' (Each xs) = Function <$> _stepSeqNoError xs
    step' (Tuple xs) = Tuple <$> _stepSeqNoError xs
    step' (Scope inner) = stepScope inner
    step' (Call x y) = stepCall x y
    step' r@(Refine a b) = do
      res <- _stepSeqNoError [a,b]
      let [new_a, new_b] = res
      return $ Refine new_a new_b
    step' c@(Has x y) = stepHas x y
    step' b@(Bind lhs rhs) =
      if isRecursive b
        then return b
        else bind lhs rhs
    step' (Select a ids) = stepSelect a ids
    -- step' (Hide a ids) = stepHide a ids
    step' (Unique h a) = Unique h <$> step a
    step' (Context env s) = do
      pushEnv env
      step' s
      newEnv <- popEnv
      return (Context newEnv s)

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

evaluate :: MountainTerm -> MountainContextT IO MountainTerm
evaluate t = do
  res <- step t
  if res == t
    then return res
    else evaluate res

-- Terms must be normalized
bind :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
bind r@(Reference n) b = do
  putDef n b
  return b
bind (Literal Wildcard) b = return b
bind a@(Literal Star) b = throwError $ NotImplemented $ Bind a b
bind a@(Literal x) b@(Literal y) =
  if x == y
    then return a
    else throwError $ BadBind a b
bind a@(Literal All) b = throwError $ BadBind a b
bind a@(Literal (Thing n)) b =
  if a == b
    then return b
    else throwError $ BadBind a b
bind a@(Import _) b = throwError $ BadBind a b
bind a b@(Import _) = throwError $ BadBind a b
bind a@(Set elems) b@(Set elems2) =
  if a == b
    then return b
    else throwError $ BadBind a b
bind a@(Set elems) b = throwError $ BadBind a b
bind a@(Function []) b@(Function []) = return b
bind a@(Function (x:xs)) b@(Function (y:ys)) =
  return $ Function [Bind x y, Bind (Function xs) (Function ys)]
bind a@(Function _) b = throwError $ BadBind a b
bind a@(Tuple xs) b@(Tuple ys) =
  return $ Tuple $ zipWith Bind xs ys
bind a@(Tuple _) b = throwError $ BadBind a b
bind a@(Either []) b = throwError $ BadBind a b
bind a@(Either inner) b = return $ Either $ map (`Bind` b) inner
bind a@(Each []) b = throwError $ BadBind a b
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
bind a@(Bind x y) b = do
  bind x y
  bind x b
bind a b@(Bind x y) = do
  res <- bind x y
  bind a res
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
has  a (Reference n) = do
  d <- getDef n
  return $ Has a d
-- left associative rules
has a@(Literal All) b = return b
has a@(Literal Wildcard) b = return b
has a@(Literal (Thing _)) b = throwError $ BadHas a b
has a@(Literal Star) b = throwError $ InvalidTerm $ Has a b
has (Reference n) b = do
  d <- getDef n
  return $ Has d b
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
      stepa <- stepRec a
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
select a b = throwError $ BadSelect a b

call :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
call a@(Function []) b = throwError $ BadCall $ Call a b
call a@(Function [_]) b = throwError $ BadCall $ Call a b
call a@(Function (x:xs)) b = do
  pushEnv M.empty
  res <- bind x b
  new_env <- popEnv
  return $ Context new_env $ Function xs
call a@(Either []) b = throwError EmptyEither
call a@(Either xs) b = return $ Either $ map (`Call` b) xs
call a@(Each []) b = throwError $ BadCall $ Call a b
call a@(Each xs) b = return $ Each $ map (`Call` b) xs
call a b = error "bad evaluate call"

stepCall :: MountainTerm -> MountainTerm -> MountainContextT IO MountainTerm
stepCall x y = do
  call x y
  `catchError` (\case
      BadCall _ -> do
        step_x <- step x
        step_y <- step y
        if step_x == x && step_y == y
          then throwError $ BadCall $ Call x y
          else return $ Call step_x step_y
      other -> throwError other
    )

stepScope :: [MountainTerm] -> MountainContextT IO MountainTerm
stepScope [] = throwError EmptyScope
stepScope [x] = return x
stepScope (x:xs) = do
  res <- step x
  if res == x
    then return $ Scope xs
    else return $ Scope (res:xs)

-- Basic Interpreter makers
type MountainImporter = FilePath -> MountainContextT IO MountainTerm

dummyImporter :: MountainImporter
dummyImporter c = return empty

dotImportFile :: FilePath -> String -> MountainImporter -> FilePath -> MountainContextT IO MountainTerm
dotImportFile base_path file_ext importer fp =
    let
        repl '.' = '/'
        repl c = c
        file_name = base_path ++ map repl fp

        removeFileExt :: FilePath -> FilePath
        removeFileExt str = if str == file_ext then "" else  head str:removeFileExt (tail str)
    in do
        file_exist <- lift $ doesFileExist (file_name ++ file_ext)
        dir_exist <- lift $ doesDirectoryExist file_name
        if file_exist
            then importer (file_name ++ file_ext)
        else if dir_exist
            then do
                dirContents <- lift $ listDirectory file_name
                let baseDirContents = sort $ map removeFileExt dirContents
                let loadedDir = map ((fp ++ ".") ++) baseDirContents
                let zipped_dir = zip baseDirContents loadedDir
                return $ Tuple $ map (\(ref, path) -> Bind (Reference ref) ((Import . Reference) path)) zipped_dir
        else
          if not file_exist
            then throwError $ FileNotExist $ Reference fp
          else if not dir_exist
            then throwError $ DirNotExist $ Reference file_name
          else error "should not reach here"

evaluateImport :: MountainImporter -> MountainTerm -> MountainContextT IO MountainTerm
evaluateImport importer (Import i@(Import _)) = evaluateImport importer i
evaluateImport importer (Import r@(Reference n)) = importer n
evaluateImport importer i@(Import a@(Select bc ids)) = do
  res <- evaluateImport importer bc
  return $ Select res ids
-- evaluateImport importer i@(Import a@(Hide bc ids)) = do
--   res <- evaluateImport importer bc
--   return $ Select res ids
evaluateImport importer i@(Import c@(Tuple inner)) = do
  result <- mapM (evaluateImport importer . Import) inner
  return $ Tuple result
evaluateImport importer i@(Import (Bind n pc)) = do
  result <- evaluateImport importer (Import pc)
  return $ Bind n result
evaluateImport importer something = error $ "what is this import case? " ++ show something
