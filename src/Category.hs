{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Category where

import Data.List ( intercalate, find, sort )

import Data.Either
import Data.Dynamic
import Data.Typeable

import qualified Data.Map as Map

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix (takeBaseName)
import Control.Monad
import Control.Monad.Except
    ( runExceptT,
      MonadTrans(..),
      MonadError(..),
      MonadIO,
      ExceptT(..) )
import Control.Monad.Writer.Strict hiding (Any)
import Control.Monad.Identity

import Debug.Trace (trace)

{- add specific name for symbolic execution cases-}
data Id =
    Name [Char] |
    Unnamed
    deriving (Eq, Show, Read)

data SpecialType =
    Flexible |
    Any
    deriving (Eq, Show, Read)

data CompositeType =
    Tuple | -- tuple, Ancategory that has each of its inner categories separably via its index
    Either | -- union, A category that has at least one of its inner categories. Inner Labeled categories are indexable
    Function | -- a function, a category that has a dependency on another category
    Composition | -- a chain of functions (a->b, b->c = a->c)
    Match -- a case statement of functions     (a->b, b->c = a->c)
    deriving (Eq, Show, Read)

data PlaceholderType =
    Label | -- a way of referring to categories by name
    Variable | -- a way of referring to categories contained by the category
    Resolved  -- the way of marking a label as having been resolved to a category
    deriving (Eq, Show, Read)

data AccessType =
  ByIndex Int |
  ByLabelGroup [Id] |
  Subtractive [Id]
  deriving (Eq, Show, Read)

data Category =
    -- categories
      Thing { name::Id } -- a concrete object
    | Set {elements::[Category]}
    | Unique {
        unique_id::Id,
        inner_category::Category
    }
    | Composite {  -- a group of things
        composite_type::CompositeType,
        inner_categories::[Category]
    }
    | Refined {
        base::Category,
        predicate::Category -- specifically predicates are morphisms of Category -> Bool
    }
    | Special {
        special_type::SpecialType
    }
    -- language constructs
    | Placeholder {
        name::Id,
        placeholder_kind::PlaceholderType,
        placeholder_category::Category
    }
    | Reference {
        name::Id
    }
    | Call {
        base::Category,
        argument::Category
    }
    | Access {
        base::Category,
        access_type::AccessType
    }
    | Import { -- TODO MAKE ON REFERENCE & TUPLE of REFS
        import_category::Category
    }
    | TypeAnnotation {
        big_category::Category,
        small_category::Category
    }
    | Scope {
        statements::[Category]
    }
    | Binding {
        placeholder::Category,
        category_to_bind::Category
    }
    deriving (Eq, Show, Read)

-- Messages
data StepMsgType =
    DeeperEval |
    CaughtErrorDeeperEval |
    Simplifying |
    Returning |
    Calling |
    SingleCompositeUnwrap |
    SingleDefUnwrap |
    Importing |
    CallingImporter |
    ImporterReturn |
    ReducingComposite |
    TypeAnnotationCheck |
    Accessing |
    MappingOver |
    Relabeling
    deriving (Eq, Show, Read)

data HasMsgType =
  EqualResult |
  HasResult |
  UseTypeAnnotation |
  UsePlaceholderCategory |
  UnrollLabel |
  ExtractFlatMapping |
  MapEvaluateInner |
  Transform |
  Evaluate
  deriving (Eq, Show, Read)

-- basic functions
data BigOrSmall = Big | Small | Both deriving Show
data CategoryLog =
  Step {
    msg::StepMsgType,
    input :: [Category]
  } |
  Has {
    has_msg::HasMsgType,
    on_which::BigOrSmall,
    big::Category,
    small::Category
  } |
  Output {
    output::Category
  }
  deriving (Show)

-- Error types
data ErrorType =
    EmptyAccessBase |
    EmptyAccessID |
    BadAccess |
    UndefinedAccess |
    RefinementNotHandledInAccess |
    UnresolvedReference |
    CallingADataCompositeCategory |
    UnnamedCategory |
    EmptyFunctionalComposite |
    InsufficientFunctionTerms |
    DataInFunctionComposite |
    BadCallInMatch |
    BadCall |
    InvalidArgument |
    PredicateHasNonFunctionArgument |
    NonFunctioninCallBase |
    BadRefinementPredicateInput |
    BadImport |
    BadExportFileExists |
    CannotTypecheckRawImport |
    CannotTypecheckRawDefinition |
    BadTypeAnnotation |
    EmptyScope |
    NoReturnInScope |
    BadBinding |
    BindingOutsideOfScope |
    IsNotLabeled |
    BadRelabel |
    NestedImport
    deriving (Eq, Show, Read)

data Error = Error {
    error_type::ErrorType,
    error_stack::[Category]
} deriving (Eq, Show, Read)

addCategoryToErrorStack :: Category -> Error -> Error
addCategoryToErrorStack input_a e@Error{error_stack=stack} = e{error_stack=stack++[input_a]}

newtype CategoryContextT m a = CategoryContextT { getCategoryContext :: ExceptT [Error] (WriterT [CategoryLog] m) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError [Error], MonadWriter [CategoryLog])

instance MonadTrans CategoryContextT where
    lift = CategoryContextT . lift . lift

type CategoryContext = CategoryContextT Identity

runCategoryContextT :: CategoryContextT m a -> m (Either [Error] a, [CategoryLog])
runCategoryContextT s = runWriterT (runExceptT (getCategoryContext s))

runCategoryContext :: CategoryContext a -> (Either [Error] a, [CategoryLog])
runCategoryContext s = runIdentity $ runCategoryContextT s

identityToIO :: CategoryContext a -> CategoryContextT IO a
identityToIO category = CategoryContextT $ ExceptT $ WriterT $ return $ runCategoryContext category

type CategoryImporter = FilePath -> CategoryContextT IO Category

data CategoryEvalOptions = Options {
  reduce_composite :: Bool,
  importer :: CategoryImporter
}

getLogOfT :: (Monad m) => CategoryContextT m Category -> m [CategoryLog]
getLogOfT c = snd <$> runCategoryContextT c

getLogOf :: CategoryContext a -> [CategoryLog]
getLogOf c = snd $ runCategoryContext c

getResultOfT :: (Monad m) => CategoryContextT m a -> m (Either [Error] a)
getResultOfT c = fst <$> runCategoryContextT c

getResultOf :: CategoryContext a -> Either [Error] a
getResultOf c = fst $ runCategoryContext c

-- useful categories

valid :: Category
valid = Composite Tuple []

empty :: Category
empty = Composite Either []

universal :: Category
universal = Special{special_type=Any}

flex :: Category
flex = Special{special_type=Flexible}

-- checks for data constructor type
isName :: Id -> Bool
isName Name{} = True
isName _ = False

isThing :: Category -> Bool
isThing Thing{} = True
isThing _ = False

isComposite :: Category -> Bool
isComposite Composite{} = True
isComposite _ = False

isCompositeOfType :: CompositeType -> Category -> Bool
isCompositeOfType input_comp_type Composite{composite_type=cmp_comp_type} = input_comp_type == cmp_comp_type
isCompositeOfType _ _ = False

isDataCompositeType :: CompositeType -> Bool
isDataCompositeType Tuple = True
isDataCompositeType Either = True
isDataCompositeType _ = False

isFunctionCompositeType :: CompositeType -> Bool
isFunctionCompositeType = not . isDataCompositeType

isDependentCompositeType :: CompositeType -> Bool
isDependentCompositeType Function = True
isDependentCompositeType Tuple = True
isDependentCompositeType _ = False

isFunctionComposite :: Category -> Bool
isFunctionComposite Composite{composite_type=c_type, inner_categories=inner}
    | isFunctionCompositeType c_type = True
    | isDataCompositeType c_type && length inner == 1 = isFunctionComposite (head inner)
    | otherwise = False
isFunctionComposite _ = False

isPlaceholder :: Category -> Bool
isPlaceholder Placeholder{} = True
isPlaceholder _ = False

isPlaceholderType :: PlaceholderType -> Category -> Bool
isPlaceholderType inp_ph_type Placeholder{placeholder_kind=ph_type} = inp_ph_type == ph_type
isPlaceholderType _ _ = False

isLabelOfName :: Id -> Category -> Bool
isLabelOfName id p@Placeholder{name=name, placeholder_kind=Label} = name == id
isLabelOfName id other = False

isCall :: Category -> Bool
isCall Call{} = True
isCall _ = False

isReference :: Category -> Bool
isReference Reference{} = True
isReference _ = False

isReferenceOfName :: Id -> Category -> Bool
isReferenceOfName id r@Reference{name=n} = id == n
isReferenceOfName _ _ = False

isSpecial :: Category -> Bool
isSpecial Special{} = True
isSpecial _ = False

isSpecialType :: SpecialType -> Category -> Bool
isSpecialType sctype Special{special_type=c_type} = sctype == c_type
isSpecialType sctype other = False

isAccess :: Category -> Bool
isAccess Access{} = True
isAccess _ = False

isRefined :: Category -> Bool
isRefined Refined{} = True
isRefined _ = False

isImport :: Category -> Bool
isImport Import{} = True
isImport _ = False

isNamedCategory :: Category -> Bool
isNamedCategory Thing{} = True
isNamedCategory Placeholder{} = True
isNamedCategory Reference{} = True
isNamedCategory Import{import_category=c} = isNamedCategory c
isNamedCategory _ = False

getName :: Category -> Maybe Id
getName Import{import_category=c} = getName c
getName input_category
    | not $ isNamedCategory input_category = Nothing
    | otherwise = Just $ name input_category
-- end checks for data constructor type

-- AST manipulation

checkAST :: ([Bool] -> Bool) -> (Category -> Bool) -> Category -> Bool
checkAST aggregator checker t@Thing{} = checker t
checkAST aggregator checker s@Set{elements=e} = aggregator $ checker s : map checker e
checkAST aggregator checker t@Unique{inner_category=ic} = checker ic
checkAST aggregator checker c@Composite{inner_categories=inner} = aggregator $ checker c : map (checkAST aggregator checker) inner
checkAST aggregator checker p@Placeholder{placeholder_category=ph_c} = aggregator [checker p, checkAST aggregator checker ph_c]
checkAST aggregator checker r@Refined{base=b, predicate=p} = aggregator (checker r : map (checkAST aggregator checker) [b,p])
checkAST aggregator checker s@Special{} = checker s
checkAST aggregator checker r@Reference{} = checker r
checkAST aggregator checker fc@Call{base=b, argument=a} = aggregator (checker fc: map (checkAST aggregator checker) [b,a])
checkAST aggregator checker a@Access{base=b, access_type=id} = aggregator [checker a, checkAST aggregator checker b]
checkAST aggregator checker i@Import{} = checker i
checkAST aggregator checker mem@TypeAnnotation{big_category=bc, small_category=sc} = aggregator (checker mem: map (checkAST aggregator checker) [bc,sc])
checkAST aggregator checker s@Scope{statements=st} = aggregator $ checker s : map checker st
checkAST aggregator checker b@Binding{placeholder=ph, category_to_bind=c} = aggregator [checker b, checker ph, checker c]

{- This transforms the AST according to an input mapping -}
data TransformResult a =
  Result a |
  Recurse a
  deriving (Eq, Show)

transformAST :: (Monad m) => (Category -> TransformResult (m Category)) -> Category -> m Category
transformAST transformer input_category = do
  case transformer input_category of
    Result category -> category
    Recurse m_category -> do
      category <- m_category
      let recurse = transformAST transformer
      case category of
        t@Thing{} -> return t
        s@(Set ic) -> Set <$> mapM recurse ic
        u@(Unique id ic) -> Unique id <$> recurse ic
        s@(Special t) -> return s
        r@(Reference n) -> return r
        c@(Composite c_type inner) -> Composite c_type <$> mapM recurse inner
        p@(Placeholder n p_type ph_c) -> Placeholder n p_type <$> recurse ph_c
        r@(Refined b p) -> Refined <$> recurse b <*> recurse p
        fc@(Call b a) -> Call <$> recurse b <*> recurse a
        a@(Access b access_type) -> Access <$> recurse b <*> pure access_type
        i@(Import i_c) -> Import <$> recurse i_c
        m@(TypeAnnotation b s) -> TypeAnnotation <$> recurse b <*> recurse s
        s@Scope{statements=st} -> Scope <$> mapM recurse st
        b@Binding{placeholder=ph, category_to_bind=c} -> Binding <$> recurse ph <*> recurse c
-- {- Category Functions -}

isRecursiveCategory :: Category -> CategoryContext Bool
isRecursiveCategory c = do
  new_c <- normalize c
  case new_c of
    Placeholder{name=ph_name, placeholder_kind=Label, placeholder_category=ph_c} -> return $ checkAST or (isReferenceOfName ph_name) ph_c
    _ -> return False

replaceReferences :: Id -> Category -> Category -> CategoryContext Category
replaceReferences ref_name new_cat base_cat =
    let
        replaceRefTransformer :: Category -> TransformResult (CategoryContext Category)
        replaceRefTransformer p@Placeholder{name=ph_n, placeholder_kind=Label}
            | ph_n == ref_name = Result $ return p
            | otherwise = Recurse $ return p
        replaceRefTransformer r@Reference{name=n} =
          if n == ref_name
            then Result $ return (Placeholder n Resolved new_cat)
            else Recurse $ return r
        replaceRefTransformer c = Recurse $ return c
    in
      transformAST replaceRefTransformer base_cat

resolveReferences :: Category -> Category
resolveReferences cat =
    let
        resolveReferencesTransformer :: Category -> TransformResult (Identity Category)
        resolveReferencesTransformer rr@Placeholder{placeholder_kind=Resolved, placeholder_category=ph_c} = Result $ return ph_c
        resolveReferencesTransformer c = Recurse $ return c
    in
      runIdentity $ transformAST resolveReferencesTransformer cat

relabel :: Id -> Category -> CategoryContext Category
relabel new_name p2@(Placeholder n2 Label ph_c2) =
    let
      relabelTransformer :: Category -> TransformResult (CategoryContext Category)
      relabelTransformer label1@(Placeholder k Label ph_ck) =
        if k == new_name
          then do
            let new_phck = replaceReferences k (Reference new_name) ph_ck
            Result $ Placeholder new_name Label <$> new_phck
          else Recurse $ return label1
      relabelTransformer k = Recurse $ return k
    in
      transformAST relabelTransformer p2
relabel n c = throwError [Error BadRelabel [Reference n, c]]

data UnrollType = Flat | Recursive
unroll :: UnrollType -> Category -> CategoryContext Category
unroll t l@Placeholder{name=rec_name, placeholder_kind=Label, placeholder_category=rec_cat} = do
  isRecursive <- isRecursiveCategory l
  if isRecursive
    then normalize l >>= unroll_inner t
    else return rec_cat
  where
    unroll_inner :: UnrollType -> Category -> CategoryContext Category
    unroll_inner Flat l@Placeholder{name=rec_name, placeholder_kind=Label, placeholder_category=rec_cat} =
        replaceReferences rec_name flex rec_cat
    unroll_inner Recursive l@Placeholder{name=rec_name, placeholder_kind=Label, placeholder_category=rec_cat} =
        replaceReferences rec_name l rec_cat
    unroll_inner _ other = return other
unroll _ something_else = return something_else

getInnerBindings :: Category -> CategoryContext [(Category, Category)]
getInnerBindings c@(Composite Tuple inner) = concat <$> mapM getInnerBindings inner
getInnerBindings p@(Placeholder n Label ph_cat) = do
  res <- unroll Recursive p
  (bind_result, bindings) <- getBindings (Reference n) res
  if not bind_result
    then throwError [Error{error_type=BadBinding, error_stack=[p]}]
    else return bindings
getInnerBindings k = return []

getBindings :: Category -> Category -> CategoryContext (Bool, [(Category, Category)])
getBindings bind_term potential_bind = do
  nbind_term <- normalize bind_term
  npot_bind <- normalize potential_bind
  bind_inner nbind_term npot_bind
  where
    bind_inner :: Category -> Category -> CategoryContext (Bool, [(Category, Category)])
    bind_inner unique@(Unique id c) other@(Unique id2 c2) =
      if unique == other
        then return (True, [])
        else getBindings c c2 -- TODO make everything underneath const
    bind_inner c@(Composite Function (x:xs)) c2@(Composite Function (y:ys)) = do
      (res, bindings) <- getBindings x y
      bound_result <- applyBindings bindings xs
      (res2, new_bindings) <- getBindings (Composite Function xs) (Composite Function ys)
      return (res && res2, bindings ++ new_bindings)
    bind_inner r@(Reference (Name "*")) c2@(Composite Tuple inner2) = do
      result <- mapM (bind_inner r) inner2
      return (any fst result, concatMap snd result)
    bind_inner r@(Reference (Name "*")) p@(Placeholder n Label ph_c) = do
      unrolled <- unroll Recursive p
      return (True, [(Reference n, unrolled)])
    bind_inner c@(Composite Tuple inner) c2@(Composite Tuple inner2) = do
      if length inner /= length inner2
        then return (False, [])
        else getBindings (Composite Function inner) (Composite Function inner2)
    bind_inner bind_term@(Placeholder n Label ph_c) potential_bind = do
      unrolled <- unroll Recursive bind_term
      (result, bindings) <- getBindings unrolled potential_bind
      return (result, (Reference n, potential_bind) : bindings)
    bind_inner bind_term@(Placeholder n Variable ph_c) potential_bind = do
      result <- ph_c `has` potential_bind
      if result
        then do
          return (True, [(Reference n, potential_bind)])
        else return (False, [])
    bind_inner r@(Reference a) p@(Placeholder n Label ph_c) = do
      if n == a
        then return (True, [(Reference a, ph_c)])
        else do
          result <- relabel a p
          return (True, [(Reference a, result)])
    bind_inner (Reference a) other = return (True, [(Reference a, other)])
    bind_inner (Special Flexible) other = return (True, [])
    bind_inner a b = return (a == b, [])

applyBindings :: [(Category, Category)] -> [Category] -> CategoryContext [Category]
applyBindings bindings categories = do
  let bind_replace_foos = map (\(Reference n, binding) -> replaceReferences n binding) bindings
  -- on each category call bind_replace foos
  let apply_all_bindings_on = foldr (>=>) return bind_replace_foos
  mapM apply_all_bindings_on categories

call :: Category -> Category -> CategoryContext Category
call arg foo = do
  n_arg <- normalize arg
  n_foo <- normalize foo
  call_inner n_arg n_foo
  where
    call_inner :: Category -> Category -> CategoryContext Category
    call_inner arg p@Placeholder{placeholder_kind=Label} = do
      unrolled <- unroll Recursive p
      call arg unrolled
    call_inner arg c@Composite{composite_type=Function, inner_categories=head:tail} = do
      (bind_result, bindings) <- getBindings head arg
      if not bind_result
        then throwError [Error{error_type=InvalidArgument, error_stack=[arg, c]}]
        else do
          applied_bindings <- applyBindings bindings tail
          case applied_bindings of
              [] -> throwError [Error{error_type=InsufficientFunctionTerms, error_stack=[arg, c]}]
              [something] -> return something
              other -> return c{inner_categories=other}
    call_inner arg c@Composite{composite_type=Match, inner_categories=inner} = do
        let result = rights $ map (fst . runCategoryContext . call arg) inner
        case result of
            x:xs -> return x
            [] -> throwError [Error{error_type=BadCallInMatch, error_stack=[arg, c]}]
    call_inner arg c@Composite{composite_type=Composition, inner_categories=head:rest} = do
        result <- call arg head
        case rest of
          [] -> return result
          [something] -> call result something
          longer_list -> call result $ Composite Composition longer_list
    call_inner arg c = throwError [Error{error_type=BadCall, error_stack=[arg, c]}]

flatten :: Category -> CategoryContext Category
flatten t@Thing{} = return t
flatten c@Composite{composite_type=Function, inner_categories=inner} = return c
flatten c@Composite{composite_type=Composition, inner_categories=inner} = do
    head_type <- flatten $ head inner
    last_type <- flatten $ last inner
    case (head_type, last_type) of
      (Composite{composite_type=Function, inner_categories=inner_head:_}, Composite{composite_type=Function, inner_categories=tmp}) -> return Composite{composite_type=Function, inner_categories=[inner_head, last tmp]}
      _ -> throwError [Error{error_type=DataInFunctionComposite, error_stack=[c]}]
flatten c@Composite{composite_type=Match, inner_categories=inner} = do
    result <- mapM flatten inner
    let inputs = Composite Either $ map (head . inner_categories) result
    let outputs = Composite Either $ map (last . inner_categories) result
    return Composite{composite_type=Function, inner_categories=[inputs, outputs]}
flatten c@Composite{composite_type=c_type, inner_categories=inner} = Composite c_type <$> mapM flatten inner
flatten p@Placeholder{name=ph_name, placeholder_kind=p_type, placeholder_category=ph_c} = Placeholder ph_name p_type <$> flatten ph_c
flatten r@Refined{base=b, predicate=p} = Refined <$> flatten b <*> flatten p
flatten fc@Call{base=b, argument=a} = Call <$> flatten b <*> flatten a
flatten a@Access{base=b, access_type=a_type} = Access <$> flatten b <*> return a_type
flatten mem@TypeAnnotation{big_category=bc, small_category=sc} = TypeAnnotation <$> flatten bc <*> flatten sc
flatten other = return other

evaluateScope :: Category -> CategoryContext Category
evaluateScope (Scope []) = throwError [Error{error_type=EmptyScope, error_stack=[]}]
evaluateScope (Scope [s]) = return s
evaluateScope (Scope (b@(Binding big c):rest)) = do
  (valid, bindings) <- getBindings big c
  if not valid
    then throwError [Error{error_type=BadBinding, error_stack=[b]}]
    else do
      applied_bindings <- applyBindings bindings rest
      case applied_bindings of
        [] -> throwError [Error{error_type=NoReturnInScope, error_stack=[b]}]
        [something] -> return something
        many -> evaluateScope $ Scope many
evaluateScope (Scope (l@(Placeholder n Label phc):rest)) =
  evaluateScope $ Scope $ Binding (Reference n) phc:rest
evaluateScope (Scope (_:rest)) = evaluateScope $ Scope rest
evaluateScope other = error $ "bad use for reduce Statement with " ++ show other


evaluateAccess :: Category -> CategoryContext Category
evaluateAccess a@Access{base=Composite{inner_categories=[]}} =
    throwError [Error{error_type=EmptyAccessBase,error_stack=[a]}]
evaluateAccess a@Access{base=Composite{inner_categories=[something]}} = evaluateAccess a{base=something}
evaluateAccess a@Access{base=c@Composite{composite_type=c_type, inner_categories=inner}, access_type=ByLabelGroup labels} = do
  result <- Composite c_type <$> dependentAccess labels inner
  case result of
    Composite _ [s] -> return s
    k -> return k
  where
    dependentAccess :: [Id] -> [Category] -> CategoryContext [Category]
    dependentAccess [] c = return []
    dependentAccess ids [] = throwError [Error BadAccess (map Reference ids)]
    dependentAccess ids (head:rest) = do
      bindings <- getInnerBindings head
      applied <- applyBindings bindings rest
      case getName head of
        Just n -> do
          if n `elem` ids
            then do
              (head :) <$> dependentAccess (filter (/= n) ids) applied
            else dependentAccess ids applied
        Nothing -> dependentAccess ids applied
evaluateAccess a@Access{base=c@Composite{composite_type=c_type, inner_categories=inner}, access_type=Subtractive bad_labels} =
  Composite c_type <$> minusDependentAccess bad_labels inner
  where
    minusDependentAccess :: [Id] -> [Category] -> CategoryContext [Category]
    minusDependentAccess [] c = return c
    minusDependentAccess ids [] = return []
    minusDependentAccess ids (head:rest) = do
      bindings <- getInnerBindings head
      applied <- applyBindings bindings rest
      case getName head of
        Just n -> do
          if n `elem` ids
            then minusDependentAccess ids applied
            else (head :) <$> minusDependentAccess ids applied
        Nothing -> minusDependentAccess ids applied
evaluateAccess a@Access{base=Special{special_type=Flexible}, access_type=output} = return a
evaluateAccess a@Access{base=p@Placeholder{placeholder_kind=Label, placeholder_category=ph_c}, access_type=id} = do
    new_base <- unroll Recursive p
    evaluateAccess a{base=new_base}
evaluateAccess a@Access{base=p@Placeholder{placeholder_kind=Resolved, placeholder_category=ph_c}, access_type=id} =
  evaluateAccess a{base=ph_c}
evaluateAccess a@Access{base=Refined{base=b_cat, predicate=p}, access_type=id} =
        {- TODO: handle predicates -}
        throwError [Error RefinementNotHandledInAccess [a]]
evaluateAccess a@Access{base=f@Call{base=b, argument=arg}, access_type=output} = do
    result <- call arg b
    evaluateAccess a{base=result}
evaluateAccess a@Access{base=a_inner@Access{}, access_type=id} = do
    result <- evaluateAccess a_inner
    evaluateAccess a{base=result}
evaluateAccess a@Access{base=Reference{}, access_type=id} = throwError [Error BadAccess [a]]
evaluateAccess a@Access{} = throwError [Error{error_type=UndefinedAccess,error_stack=[a]}]
evaluateAccess other = error $ "Cannot evaluate access on non access category : " ++ show other

data CategoryLevel =
    Infinite |
    AnyLevel |
    LessThan Integer |
    Specific Integer
    deriving (Eq, Show, Read)

instance Ord CategoryLevel where
    (compare) (Specific a) (Specific b)  = compare a b
    (compare) Infinite Infinite = EQ
    (compare) Infinite _ = GT
    (compare) _ Infinite = LT
    (compare) (LessThan a) (LessThan b) = compare (Specific a) (Specific b)
    (compare) (LessThan a) (Specific b) = compare (Specific a) (Specific b)
    (compare) (Specific a) (LessThan b) = compare (Specific a) (Specific b)
    (compare) AnyLevel _ = EQ
    (compare) _ AnyLevel = EQ

-- level :: Category -> CategoryContext CategoryLevel
-- level input_category =
--     let
--         levelOfCategoryList :: [Category] -> CategoryContext CategoryLevel
--         levelOfCategoryList [] = return $ Specific 0
--         levelOfCategoryList inner_categories = do
--             result <- mapM level inner_categories
--             case result of
--               [] -> return AnyLevel
--               _ -> return $ maximum result

--         incrementLevel :: CategoryLevel -> CategoryLevel
--         incrementLevel some_level =
--             case some_level of
--                 Specific l -> Specific (1 + l)
--                 anything_else -> anything_else

--         level_inner :: Category -> CategoryContext CategoryLevel
--         level_inner input_category =
--             case input_category of
--                 (Thing t) -> return $ Specific 0
--                 (Composite _ inner) -> levelOfCategoryList inner
--                 p@Placeholder{placeholder_kind=Variable, placeholder_category=ph_c} -> do
--                   result <- level ph_c
--                   case result of
--                       (Specific l) -> return (LessThan l)
--                       (LessThan l) -> return (LessThan l)
--                       Infinite -> return AnyLevel
--                       AnyLevel -> return AnyLevel
--                 l@Placeholder{placeholder_kind=Label, placeholder_category=ph_c} ->
--                     let
--                         category_of_interest =
--                             if isRecursiveCategory ph_c
--                                 then unroll Flat l
--                                 else ph_c
--                     in level category_of_interest
--                 rr@Placeholder{placeholder_kind=Resolved, placeholder_category=ph_c} -> level ph_c
--                 Refined {base=_base_category, predicate=_predicate} -> level _base_category
--                 Special{special_type=Flexible} -> return AnyLevel
--                 Special{special_type=Any} -> return Infinite
--                 Reference{} -> return AnyLevel
--                 Call{base=b, argument=a} -> call a b >>= level
--                 a@Access{base=bc,access_id=id} -> evaluateAccess a >>= level
--                 i@Import{} -> return AnyLevel
--                 def@Definition{def_category=d} -> level d
--                 mem@TypeAnnotation{big_category=bc, small_category=sc} -> level sc
--     in
--         if isRecursiveCategory input_category || isCompositeOfType Either input_category
--             then do
--               result <- level_inner input_category
--               return (incrementLevel result)
--             else level_inner input_category


debugTell :: (Show w, MonadWriter w m) => w -> m ()
-- debugTell x = trace (show x) (tell x)
debugTell x = tell x

normalize :: Category -> CategoryContext Category
normalize (Composite _ [category]) = normalize category
normalize (Placeholder _ Resolved category) = normalize category
normalize s@(Scope _) = evaluateScope s
normalize x = return x

fullNormalize :: Category -> CategoryContext Category
fullNormalize c = do
  x <- normalize c
  if x == c
    then return x
    else fullNormalize x

has :: Category -> Category -> CategoryContext Bool
has big_category small_category = do
    normalized_big <- normalize big_category
    normalized_small <- normalize small_category

    if big_category == small_category
      then do
        debugTell  [Has{has_msg=EqualResult,on_which=Both,big=big_category,small=small_category}]
        return False
      else has_inner normalized_big normalized_small
        where
          has_inner :: Category -> Category -> CategoryContext Bool
          {- TypeAnnotation -}
          has_inner mem@TypeAnnotation{big_category=bc, small_category=sc} other = do
            debugTell  [Has{has_msg=UseTypeAnnotation,on_which=Big,big=big_category,small=small_category}]
            has bc other
          has_inner other mem@TypeAnnotation{big_category=bc, small_category=sc} = do
            debugTell  [Has{has_msg=UseTypeAnnotation,on_which=Small,big=big_category,small=small_category}]
            has other bc
          {- Placeholder Variable -}
          has_inner (Placeholder _ Variable category) small_category = do
            debugTell  [Has{has_msg=UsePlaceholderCategory,on_which=Big,big=big_category,small=small_category}]
            has category small_category
          has_inner big_category (Placeholder _ Variable category) = do
            if big_category == category
              then do
                debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return True
              else do
                debugTell  [Has{has_msg=UsePlaceholderCategory,on_which=Small,big=big_category,small=small_category}]
                has big_category category
          {- Placeholder Label -}
          has_inner p@(Placeholder _ Label category) small_category = do
            debugTell  [Has{has_msg=UnrollLabel,on_which=Big,big=big_category,small=small_category}]
            isRecursive <- isRecursiveCategory p
            if isRecursive
              then do
                unrolled <- unroll Recursive p
                has unrolled small_category
              else has category small_category
          has_inner big_category p@(Placeholder _ Label category) = do
            debugTell  [Has{has_msg=UnrollLabel,on_which=Small,big=big_category,small=small_category}]
            isRecursive <- isRecursiveCategory big_category
            if isRecursive
              then do
                unrolled <- unroll Recursive p
                has big_category p
              else has category small_category
          {- Composition -}
          has_inner c_big@Composite{composite_type=Composition, inner_categories=big_ic} other_category = do
            debugTell  [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            result1 <- has_inner (Composite Tuple big_ic) other_category

            cat <- flatten c_big
            result2 <- cat `has` other_category
            return $ result1 || result2
          has_inner other_category c_small@Composite{composite_type=Composition, inner_categories=small_ic} = do
            debugTell  [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            result1 <- has_inner other_category (Composite Tuple small_ic)

            cat <- flatten c_small
            result2 <- other_category `has` cat
            return $ result1 || result2
          {- Match -}
          has_inner c_big@Composite{composite_type=Match, inner_categories=big_ic} other_category = do
            debugTell  [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            result1 <- has_inner (Composite Either big_ic) other_category

            cat <- flatten c_big
            result2 <- cat `has` other_category
            return $ result1 || result2
          has_inner other_category c_small@Composite{composite_type=Match, inner_categories=small_ic} = do
            debugTell  [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            result1 <- has_inner other_category (Composite Either small_ic)

            cat <- flatten c_small
            result2 <- other_category `has` cat
            return $ result1 || result2
          {- Composite Function -}
          has_inner c1@Composite{composite_type=Function, inner_categories=c1ic} c2@Composite{composite_type=Function, inner_categories=c2ic} = do
                debugTell  [Has{has_msg=Transform,on_which=Both,big=big_category,small=small_category}]
                has (Composite Tuple c1ic) (Composite Tuple c2ic)
          has_inner c1@Composite{composite_type=Function} other = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Unique -}
          has_inner (Unique big_id big_ic) (Unique small_id small_ic) =
            if big_id /= small_id
              then return False
              else big_ic `has` small_ic
          has_inner other (Unique _ ic) = other `has` ic
          has_inner (Unique _ _) _ = return False
          {- Set -}
          has_inner c_big@(Set set_elems) e@(Composite Either either_inner) = do
            let normalized_elems = map normalize set_elems
            if e `elem` set_elems
              then do
                debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return True
              else do
                normalized_inner <- mapM normalize either_inner
                debugTell  [Has{has_msg=MapEvaluateInner,on_which=Both,big=big_category,small=small_category}]
                and <$> mapM (has c_big) normalized_inner
          has_inner c_big@(Set set_elems) other = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            normalized_elems <- mapM normalize set_elems
            return $ other `elem` normalized_elems
          {- Composite Tuple -}
          has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Tuple small_inner)
              | length big_inner /= length small_inner = do
                debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return False
              | otherwise = do
                debugTell  [Has{has_msg=MapEvaluateInner,on_which=Both,big=big_category,small=small_category}]
                and <$> zipWithM has big_inner small_inner
          has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Either small_inner) = do
            debugTell  [Has{has_msg=MapEvaluateInner,on_which=Small,big=big_category,small=small_category}]
            or <$> mapM (has c_big) small_inner
          has_inner c_big@(Composite Tuple _) other = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Composite Either -}
          has_inner (Composite Either sum_inner) other_category = do
            debugTell  [Has{has_msg=MapEvaluateInner,on_which=Big,big=big_category,small=small_category}]
            or <$> mapM (`has` other_category) sum_inner
          has_inner other_category (Composite Either sum_inner) = do
            debugTell  [Has{has_msg=MapEvaluateInner,on_which=Small,big=big_category,small=small_category}]
            and <$> mapM (has other_category) sum_inner
          {- Refined -}
          {- TODO: Handle refinements via SMT solver and approximate methods -}
          has_inner Refined{base=base, predicate=p} Refined{base=base_other, predicate=p_other} = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            and <$> sequence [has base p, has p p_other]
          has_inner Refined{base=base, predicate=p} other = do
              debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
              if base == other
                then return True
                else return False
          {- Special -}
          has_inner Special{} _ = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return True
          has_inner _ Special{special_type=Flexible} = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return True
          has_inner _ Special{special_type=Any} = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Reference -}
          has_inner r@Reference{} other = throwError [Error{error_type=UnresolvedReference, error_stack=[r]}]
          has_inner other r@Reference{} = throwError [Error{error_type=UnresolvedReference, error_stack=[r]}]
          {- Function Call -}
          has_inner bmc@Call{base=bbm, argument=ba} smc@Call{base=sbm, argument=sa} = do
            debugTell  [Has{has_msg=MapEvaluateInner,on_which=Both,big=big_category,small=small_category}]
            result <- and <$> sequence [bbm `has` sbm, ba `has` sa]
            if result
              then do
                debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return result
              else do
                debugTell  [Has{has_msg=Evaluate,on_which=Both,big=big_category,small=small_category}]
                bc <- call ba bbm
                sc <- call sa sbm
                bc `has` sc
          has_inner mc@Call{base=bm, argument=a} other = do
            debugTell  [Has{has_msg=Evaluate,on_which=Big,big=big_category,small=small_category}]
            cat <- call a bm
            has cat other
          has_inner other mc@Call{base=bm, argument=a} = do
            debugTell  [Has{has_msg=Evaluate,on_which=Small,big=big_category,small=small_category}]
            cat <- call a bm
            has other cat
          {- Access -}
          has_inner a@Access{base=b, access_type=id} other = do
            debugTell  [Has{has_msg=Evaluate,on_which=Big,big=big_category,small=small_category}]
            cat <- evaluateAccess a
            has cat other
          {- Import -}
          has_inner i@Import{} i2@Import{} = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return (i == i2)
          has_inner other i@Import{} = throwError [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
          has_inner i@Import{} other = throwError [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
          {- Things -}
          -- equal things and other cases are handled above
          has_inner (Thing t) _ = do
            debugTell  [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Statements -}
          has_inner big_category s@(Scope statements) = do
            error "should not reach here because should be normalized"
            debugTell  [Has{has_msg=Transform,on_which=Small,big=big_category,small=small_category}]
            reduced <- evaluateScope s
            has_inner big_category reduced
          has_inner s@(Scope statements) small_category = do
            error "should not reach here because should be normalized"
            debugTell  [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            reduced <- evaluateScope s
            has_inner big_category reduced
          {- Bindings -}
          has_inner big_category (Binding b other_cat) = do
            debugTell  [Has{has_msg=Transform,on_which=Small,big=big_category,small=small_category}]
            has_inner big_category b
          has_inner (Binding b other_cat) small_category = do
            debugTell  [Has{has_msg=Transform,on_which=Small,big=big_category,small=small_category}]
            has_inner b small_category
          {- Normalized away cases -}
          has_inner _ (Placeholder _ Resolved _) = error "should not reach"
          has_inner (Placeholder _ Resolved _) _ = error "should not reach"

{- This does not do any checking which requires evaluation -}
validateCategory :: Category -> CategoryContext Category
validateCategory t@(Thing Unnamed) = throwError [Error{error_type=UnnamedCategory, error_stack=[t]}]
validateCategory c@Composite{composite_type=composition_type, inner_categories=[]}
    | isFunctionCompositeType composition_type =
        throwError [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
    | otherwise = return c
validateCategory c@Composite{composite_type=composition_type, inner_categories=inner_cats}
    | (composition_type == Composition || composition_type == Match ) && not (all isFunctionComposite inner_cats) =
        throwError [Error{error_type=DataInFunctionComposite, error_stack=[c]}]
    | (composition_type == Function) && all isImport inner_cats =
        throwError [Error{error_type=InsufficientFunctionTerms, error_stack=[c]}]
    | otherwise = return c
validateCategory a@Access{base=bc, access_type=ByLabelGroup labels} =
  if Unnamed `elem` labels || null labels
    then throwError [Error{error_type=EmptyAccessID, error_stack=[a]}]
    else return a
validateCategory other = return other

validateCategoryRecursive :: Category -> CategoryContext Category
validateCategoryRecursive = transformAST (Recurse . validateCategory)

-- Basic Interpreter makers

loadModule :: FilePath -> String -> CategoryImporter -> FilePath -> CategoryContextT IO Category
loadModule module_base_path file_ext importer fp =
    let
        repl '.' = '/'
        repl c = c
        file_name = module_base_path ++ map repl fp

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
                return Composite{composite_type=Tuple, inner_categories=
                    map (\(ref, path) -> Placeholder (Name ref) Category.Label ((Import . Reference . Name) path)) zipped_dir
                }
        else throwError [Error BadImport [Reference (Name fp)]]

evaluateImport :: CategoryImporter -> Category -> CategoryContextT IO Category
evaluateImport importer imp@(Import (Import _)) = do
  throwError [Error NestedImport [imp]]
evaluateImport importer (Import r@(Reference (Name n))) = do
  debugTell [Step CallingImporter [r]]
  res <- importer n
  debugTell [Step ImporterReturn [res]]
  return res
evaluateImport importer i@(Import a@Access{base=bc, access_type=id}) =
    let
        extractFilePath :: Category -> FilePath
        extractFilePath (Reference (Name n)) = n
        extractFilePath (Access bc (ByLabelGroup [Name n])) = extractFilePath bc ++ "." ++ n
        extractFilePath _ = error "Something weird"
    in do
      debugTell [Step CallingImporter [a]]
      res <- importer (extractFilePath a)
      debugTell [Step ImporterReturn [res]]
      return res
evaluateImport importer i@(Import c@(Composite Tuple inner)) = do
  debugTell [Step MappingOver [i]]
  result <- mapM (evaluateImport importer . Import) inner
  return c{inner_categories=result}
evaluateImport importer i@(Import p@Placeholder{name=n, placeholder_kind=Category.Label, placeholder_category=pc}) = do
    debugTell [Step Importing [i]]
    result <- evaluateImport importer (Import pc)
    case result of
        p@Placeholder{placeholder_kind=label, placeholder_category=ph_c} -> do
          debugTell [Step Relabeling [i]]
          return $ p{name=n}
        _ -> do
          debugTell [Step Returning [i]]
          return $ p{placeholder_category=result}
evaluateImport importer something = error $ "what is this import case? " ++ show something

step :: CategoryEvalOptions -> Category -> CategoryContextT IO Category
step _ c@Composite{inner_categories=[something]} = do
  debugTell  [Step{msg=SingleCompositeUnwrap, input=[c]}]
  return something
step opts f@Call{base=bc, argument=a} =
    do
      debugTell  [Step{msg=Calling, input=[f]}]
      identityToIO $ call a bc
    `catchError` \error -> do
      debugTell  [Step{msg=CaughtErrorDeeperEval, input=[f]}]
      stepped_bc <- step opts bc
      stepped_a <- step opts{reduce_composite=True} a
      let step_result = Call stepped_bc stepped_a
      if stepped_bc == bc && stepped_a == a
        then throwError error
        else return step_result
step opts@Options{importer=importer} i@Import{import_category=ic} =
  do
    debugTell  [Step{msg=Importing, input=[i]}]
    evaluateImport importer i
  `catchError` \error -> do
    debugTell  [Step{msg=CaughtErrorDeeperEval, input=[i]}]
    result_ic <- step opts ic
    if result_ic == ic
        then throwError error
        else return $ Import result_ic
step opts mem@TypeAnnotation{big_category=bc, small_category=sc} = do
  debugTell  [Step{msg=TypeAnnotationCheck, input=[mem]}]
  result <- identityToIO $ has bc sc
  if result
    then return sc
    else do
      debugTell  [Step{msg=DeeperEval, input=[mem]}]
      stepped_bc <- step opts bc
      stepped_sc <- step opts sc
      let step_result = TypeAnnotation stepped_bc stepped_sc
      if stepped_bc == bc && stepped_sc == sc
        then throwError [Error{error_type=BadTypeAnnotation, error_stack=[mem]}]
        else return step_result
step opts c@Composite{composite_type=Function, inner_categories=(i@(Import something):rest)} = do
    debugTell  [Step{msg=DeeperEval, input=[c]}]
    resolved_import <- step opts i
    case resolved_import of
      Import _ -> return c{inner_categories=resolved_import:rest}
      _ -> do
        bindings <- identityToIO $ getInnerBindings resolved_import
        bound_result <- identityToIO $ applyBindings bindings rest
        case bound_result of
              [] -> throwError [Error{error_type=InsufficientFunctionTerms, error_stack=[c]}]
              [something] -> return something
              other -> return c{inner_categories=other}
step opts a@Access{base=b, access_type=a_id} = do
    debugTell  [Step{msg=Accessing, input=[a]}]
    identityToIO $ evaluateAccess a
  `catchError` \errors -> do
        debugTell  [Step{msg=DeeperEval, input=[a]}]
        stepped_b <- step opts b
        if stepped_b == b
          then throwError errors
          else do
            let result = Access stepped_b a_id
            return result
step opts rr@Placeholder{placeholder_kind=Resolved, placeholder_category=ph_c} = do
  debugTell  [Step{msg=Simplifying, input=[rr]}]
  let result = ph_c
  return result
step opts@Options{reduce_composite=reduce_composite} c@Composite{composite_type=c_type, inner_categories=inner_c} =
    if not reduce_composite || isFunctionCompositeType c_type
        then do
          debugTell  [Step{msg=Returning, input=[c]}]
          return c
        else do
          debugTell  [Step ReducingComposite [c]]
          stepped_inner <- mapM (step opts) inner_c
          return $ Composite c_type stepped_inner
step opts s@Scope{} = identityToIO $ evaluateScope s
step opts b@Binding{} = throwError [Error BindingOutsideOfScope [b]]
step _ other = do
  debugTell  [Step{msg=Returning, input=[other]}]
  return other

multipleStepEvaluate :: Int -> CategoryEvalOptions -> Category -> CategoryContextT IO Category
multipleStepEvaluate 0 opts cat = do
  debugTell  [Step{msg=Returning, input=[cat]}]
  return cat
multipleStepEvaluate steps opts cat = do
    new_cat <- step opts cat
    if new_cat == cat
      then return cat
      else multipleStepEvaluate (steps-1) opts new_cat

execute :: CategoryEvalOptions -> Category -> CategoryContextT IO Category
execute opts cat = do
    new_cat <- step opts cat
    if new_cat == cat
      then return new_cat
      else execute opts new_cat
