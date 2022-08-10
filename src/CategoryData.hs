{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CategoryData where

import Data.List ( intercalate, find, sort )
import Data.Either
import Data.Dynamic
import Data.Typeable

import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix (takeBaseName)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Identity

import Debug.Trace (trace)

{- add specific name for symbolic execution cases-}
data Id =
    Name [Char] |
    Index Int |
    Unnamed
    deriving (Eq, Show, Read)

data BuiltInType =
    Flexible |
    Universal
    deriving (Eq, Show, Read)

data CompositeType =
    Tuple | -- tuple, Ancategory that has each of its inner categories separably via its index
    Union | -- union, A category that has at least one of its inner categories. Inner Labeled categories are indexable
    Function | -- a function, a category that has a dependency on another category
    Composition | -- a chain of functions (a->b, b->c = a->c)
    Match -- a case statement of functions     (a->b, b->c = a->c)
    -- TO ADD LATER
    -- | Superposition
    -- | Subtractive
    deriving (Eq, Show, Read)

data PlaceholderType =
    Label | -- a way of referring to categories by name
    Element | -- a way of referring to categories contained by the category
    Resolved  -- the way of marking a label as having been resolved to a category
    deriving (Eq, Show, Read)

data Category =
    -- categories
      Thing { name::Id } -- a concrete object
    | Composite {  -- a group of things
        composite_type::CompositeType,
        inner_categories::[Category]
    }
    | Placeholder {
        name::Id,
        placeholder_type::PlaceholderType,
        placeholder_category::Category
    }
    | Refined {
        base::Category,
        predicate::Category -- specifically predicates are morphisms of Category -> Bool
    }
    | BuiltIn {
        special_type::BuiltInType
    }
    | Reference {
        name::Id
    }
    -- language constructs
    | FunctionCall {
        base::Category,
        argument::Category
    }
    | Access {
        base::Category,
        access_id::Id
    } 
    | Import { -- TODO MAKE ON REFERENCE & TUPLE of REFS
        import_category::Category
    } 
    | Definition {
        def_category::Category
    } 
    | TypeAnnotation {
        big_category::Category,
        small_category::Category
    }
    deriving (Eq, Show, Read)

data StepMsgType =
    DeeperEval |
    Simplifying |
    Returning |
    Calling |
    SingleCompositeUnwrap |
    SingleDefUnwrap |
    Importing |
    ApplyingDefinition |
    SkippingDefinition |
    ReducingComposite |
    TypeAnnotationCheck |
    Accessing
    deriving (Eq, Show, Read)

data HasMsgType =
  HasResult |
  UnwrapComposite |
  GoInsideDefinition |
  UseTypeAnnotation |
  UsePlaceholderCategory |
  UnrollLabel |
  ExtractFlatMapping |
  ReplaceInnerDefinitions |
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
    output::Either [Error] Category
  }
  deriving (Show)

-- Error types
data ErrorType =
    EmptyAccessBase |
    EmptyAccessID |
    BadAccess |
    IndexAccessOnFunction |
    UndefinedAccess |
    RefinementNotHandledInAccess |
    UnresolvedReference |
    -- SubstituteArgOnEmptyCategory |
    -- BadSubstituteArgs |
    CallingADataCompositeCategory |
    UnnamedCategory |
    IndexedNamed |
    EmptyFunctionalComposite |
    InsufficientFunctionTerms |
    DataInFunctionComposite |
    BadFunctionCallInMatch |
    BadFunctionCall |
    InvalidArgument |
    PredicateHasNonFunctionArgument |
    NonFunctioninFunctionCallBase |
    -- NotSubstitutableArgs |
    BadRefinementPredicateInput |
    AccessIndexBelowZero |
    AccessIndexOutsideRange |
    BadImport |
    BadExportFileExists |
    CannotTypecheckRawImport |
    CannotTypecheckRawDefinition |
    BadTypeAnnotation
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
getLogOfT c = do
  log <- snd <$> runCategoryContextT c
  result <- getResultOfT c
  return $ log ++ [Output result]

getLogOf :: CategoryContext Category -> [CategoryLog]
getLogOf c = do
  let log = snd $ runCategoryContext c
  let result = getResultOf c
  log ++ [Output result]

getResultOfT :: (Monad m) => CategoryContextT m a -> m (Either [Error] a)
getResultOfT c = fst <$> runCategoryContextT c

getResultOf :: CategoryContext a -> Either [Error] a
getResultOf c = fst $ runCategoryContext c

-- useful categories

valid :: Category
valid = Composite Tuple []

empty :: Category
empty = Composite Union []

universal :: Category
universal = BuiltIn{special_type=Universal}

flex :: Category
flex = BuiltIn{special_type=Flexible}

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
isDataCompositeType Union = True
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
isPlaceholderType inp_ph_type Placeholder{placeholder_type=ph_type} = inp_ph_type == ph_type
isPlaceholderType _ _ = False

isLabelOfName :: Id -> Category -> Bool
isLabelOfName id p@Placeholder{name=name, placeholder_type=Label} = name == id
isLabelOfName id other = False

isFunctionCall :: Category -> Bool
isFunctionCall FunctionCall{} = True
isFunctionCall _ = False

isReference :: Category -> Bool
isReference Reference{} = True
isReference _ = False

isReferenceOfName :: Id -> Category -> Bool
isReferenceOfName id r@Reference{name=n} = id == n
isReferenceOfName _ _ = False

isBuiltIn :: Category -> Bool
isBuiltIn BuiltIn{} = True
isBuiltIn _ = False

isBuiltInType :: BuiltInType -> Category -> Bool
isBuiltInType sctype BuiltIn{special_type=c_type} = sctype == c_type
isBuiltInType sctype other = False

isAccess :: Category -> Bool
isAccess Access{} = True
isAccess _ = False

isRefined :: Category -> Bool
isRefined Refined{} = True
isRefined _ = False

isImport :: Category -> Bool
isImport Import{} = True
isImport _ = False

isDefinition :: Category -> Bool
isDefinition Definition{} = True
isDefinition _ = False

isNamedCategory :: Category -> Bool
isNamedCategory Thing{} = True
isNamedCategory Placeholder{} = True
isNamedCategory Reference{} = True
isNamedCategory Import{import_category=c} = isNamedCategory c
isNamedCategory Definition{def_category=d} = isNamedCategory d
isNamedCategory _ = False

getName :: Category -> Maybe Id
getName Import{import_category=c} = getName c
getName Definition{def_category=c} = getName c
getName input_category
    | not $ isNamedCategory input_category = Nothing
    | otherwise = Just $ name input_category
-- end checks for data constructor type

-- AST manipulation

checkAST :: ([Bool] -> Bool) -> (Category -> Bool) -> Category -> Bool
checkAST aggregator checker t@Thing{} = checker t
checkAST aggregator checker c@Composite{inner_categories=inner} = aggregator $ checker c : map (checkAST aggregator checker) inner
checkAST aggregator checker p@Placeholder{placeholder_category=ph_c} = aggregator [checker p, checkAST aggregator checker ph_c]
checkAST aggregator checker r@Refined{base=b, predicate=p} = aggregator (checker r : map (checkAST aggregator checker) [b,p])
checkAST aggregator checker s@BuiltIn{} = checker s
checkAST aggregator checker r@Reference{} = checker r
checkAST aggregator checker fc@FunctionCall{base=b, argument=a} = aggregator (checker fc: map (checkAST aggregator checker) [b,a])
checkAST aggregator checker a@Access{base=b, access_id=id} = aggregator [checker a, checkAST aggregator checker b]
checkAST aggregator checker i@Import{} = checker i
checkAST aggregator checker def@Definition{def_category=d} = aggregator [checker def, checkAST aggregator checker d]
checkAST aggregator checker mem@TypeAnnotation{big_category=bc, small_category=sc} = aggregator (checker mem: map (checkAST aggregator checker) [bc,sc])

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
        s@(BuiltIn t) -> return s
        r@(Reference n) -> return r
        c@(Composite c_type inner) -> Composite c_type <$> mapM recurse inner
        p@(Placeholder n p_type ph_c) -> Placeholder n p_type <$> recurse ph_c
        r@(Refined b p) -> Refined <$> recurse b <*> recurse p
        fc@(FunctionCall b a) -> FunctionCall <$> recurse b <*> recurse a
        a@(Access b a_id) -> Access <$> recurse b <*> pure a_id
        i@(Import i_c) -> Import <$> recurse i_c
        d@(Definition d_c) -> Definition <$> recurse d_c
        m@(TypeAnnotation b s) -> TypeAnnotation <$> recurse b <*> recurse s

-- {- Category Functions -}

isRecursiveCategory :: Category -> Bool
isRecursiveCategory Placeholder{name=ph_name, placeholder_type=Label, placeholder_category=ph_c} = checkAST or (isReferenceOfName ph_name) ph_c
isRecursiveCategory _ = False

replaceReferences :: Id -> Category -> Category -> Category
replaceReferences ref_name new_cat base_cat =
    let
        replaceRefTransformer :: Category -> TransformResult (Identity Category)
        replaceRefTransformer p@Placeholder{name=ph_n, placeholder_type=Label}
            | ph_n == ref_name = Result $ return p
            | otherwise = Recurse $ return p
        replaceRefTransformer r@Reference{name=n} =
          if n == ref_name
            then Result $ return (Placeholder n Resolved new_cat)
            else Recurse $ return r
        replaceRefTransformer c = Recurse $ return c
    in
      runIdentity $ transformAST replaceRefTransformer base_cat

resolveReferences :: Category -> Category
resolveReferences cat =
    let
        resolveReferencesTransformer :: Category -> TransformResult (Identity Category)
        resolveReferencesTransformer rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} = Result $ return ph_c
        resolveReferencesTransformer c = Recurse $ return c
    in
      runIdentity $ transformAST resolveReferencesTransformer cat

data UnrollType = Flat | Recursive
unroll :: UnrollType -> Category -> Category
unroll Flat l@Placeholder{name=rec_name, placeholder_type=Label, placeholder_category=rec_cat} =
    replaceReferences rec_name BuiltIn{special_type=Flexible} rec_cat
unroll Recursive l@Placeholder{name=rec_name, placeholder_type=Label, placeholder_category=rec_cat} =
    replaceReferences rec_name l rec_cat
unroll _ something_else = something_else

call :: Category -> Category -> CategoryContext Category
call arg p@Placeholder{placeholder_type=Label} = call arg (unroll Recursive p)
call arg c@Composite{composite_type=c_type,inner_categories=[]}
    | isDataCompositeType c_type = throwError [Error{error_type=CallingADataCompositeCategory, error_stack=[c]}]
    | otherwise = throwError [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
call arg c@Composite{inner_categories=[something]} = call arg something
call arg c@Composite{composite_type=Function, inner_categories=head:tail} = do
    result <- head `has` arg
    if not result
      then throwError [Error{error_type=InvalidArgument, error_stack=[arg, c]}]
      else do
        let new_tail = case head of
                p@Placeholder{name=id} -> map (replaceReferences id arg) tail
                _ -> tail
        case new_tail of
            [something] -> return something
            [] -> throwError [Error{error_type=InsufficientFunctionTerms, error_stack=[arg, c]}]
            other -> return c{inner_categories=new_tail}
call arg c@Composite{composite_type=Match, inner_categories=inner} = do
    let result = rights $ map (fst . runCategoryContext . call arg) inner
    case result of
        x:xs -> return x
        [] -> throwError [Error{error_type=BadFunctionCallInMatch, error_stack=[arg, c]}]
call arg c@Composite{composite_type=Composition, inner_categories=head:rest} = do
    result <- call arg head
    case rest of
      [] -> return result
      [something] -> call result something
      longer_list -> call result $ Composite Composition longer_list
call arg c = throwError [Error{error_type=BadFunctionCall, error_stack=[arg, c]}]

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
    let inputs = Composite{composite_type=Union, inner_categories=map (head . inner_categories) result}
    let outputs = Composite{composite_type=Union, inner_categories=map (last . inner_categories) result}
    return $ trace (show inputs) Composite{composite_type=Function, inner_categories=[inputs, outputs]}
flatten c@Composite{composite_type=c_type, inner_categories=inner} = Composite c_type <$> mapM flatten inner
flatten p@Placeholder{name=ph_name, placeholder_type=p_type, placeholder_category=ph_c} = Placeholder ph_name p_type <$> flatten ph_c
flatten r@Refined{base=b, predicate=p} = Refined <$> flatten b <*> flatten p
flatten fc@FunctionCall{base=b, argument=a} = FunctionCall <$> flatten b <*> flatten a
flatten a@Access{base=b, access_id=a_id} = Access <$> flatten b <*> return a_id
flatten d@Definition{def_category=def} = Definition <$> flatten def
flatten mem@TypeAnnotation{big_category=bc, small_category=sc} = TypeAnnotation <$> flatten bc <*> flatten sc
flatten other = return other

evaluateAccess :: Category -> CategoryContext Category
evaluateAccess a@Access{base=Composite{inner_categories=[]}, access_id=id} =
    throwError [Error{error_type=EmptyAccessBase,error_stack=[a]}]
evaluateAccess a@Access{base=Composite{inner_categories=[something]}} = evaluateAccess a{base=something}
evaluateAccess a@Access{base=c@Composite{composite_type=c_type, inner_categories=inner}, access_id=id} =
    case id of
        Index idx ->
            if isFunctionCompositeType c_type
                then throwError [Error{error_type=IndexAccessOnFunction, error_stack=[a]}]
                else return (inner!!idx)
        Name str_name -> do
            case filter (\x -> getName x == Just (Name str_name))  inner of
                [] -> throwError [Error{error_type=BadAccess,error_stack=[a]}]
                stuff -> do
                    let result = head stuff
                    if isRecursiveCategory result
                        then return result
                        else if isPlaceholder result
                            then return $ placeholder_category result
                            else return result
        Unnamed -> throwError [Error{error_type=EmptyAccessID,error_stack=[a]}]
evaluateAccess a@Access{base=BuiltIn{special_type=Flexible}, access_id=output} = return BuiltIn{special_type=Flexible}
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Label, placeholder_category=ph_c}, access_id=id} = do
    let new_base = if isRecursiveCategory p then unroll Recursive p else ph_c
    evaluateAccess a{base=new_base}
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c}, access_id=id} =
  evaluateAccess a{base=ph_c}
evaluateAccess a@Access{base=Refined{base=b_cat, predicate=p}, access_id=id} =
        {- TODO: handle predicates -}
        throwError [Error RefinementNotHandledInAccess [a]]
evaluateAccess a@Access{base=f@FunctionCall{base=b, argument=arg}, access_id=output} = do
    result <- call arg b
    evaluateAccess a{base=result}
evaluateAccess a@Access{base=a_inner@Access{}, access_id=id} = do
    result <- evaluateAccess a_inner
    evaluateAccess a{base=result}
evaluateAccess a@Access{base=Reference{}, access_id=id} = return (BuiltIn{special_type=Flexible})
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
--                 p@Placeholder{placeholder_type=Element, placeholder_category=ph_c} -> do
--                   result <- level ph_c
--                   case result of
--                       (Specific l) -> return (LessThan l)
--                       (LessThan l) -> return (LessThan l)
--                       Infinite -> return AnyLevel
--                       AnyLevel -> return AnyLevel
--                 l@Placeholder{placeholder_type=Label, placeholder_category=ph_c} ->
--                     let
--                         category_of_interest =
--                             if isRecursiveCategory ph_c
--                                 then unroll Flat l
--                                 else ph_c
--                     in level category_of_interest
--                 rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} -> level ph_c
--                 Refined {base=_base_category, predicate=_predicate} -> level _base_category
--                 BuiltIn{special_type=Flexible} -> return AnyLevel
--                 BuiltIn{special_type=Universal} -> return Infinite
--                 Reference{} -> return AnyLevel
--                 FunctionCall{base=b, argument=a} -> call a b >>= level
--                 a@Access{base=bc,access_id=id} -> evaluateAccess a >>= level
--                 i@Import{} -> return AnyLevel
--                 def@Definition{def_category=d} -> level d
--                 mem@TypeAnnotation{big_category=bc, small_category=sc} -> level sc
--     in
--         if isRecursiveCategory input_category || isCompositeOfType Union input_category
--             then do
--               result <- level_inner input_category
--               return (incrementLevel result)
--             else level_inner input_category


has :: Category -> Category -> CategoryContext Bool
has big_category small_category
    | big_category == small_category = do
      debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
      return True
    | otherwise = has_inner big_category small_category
        where
          debugTell :: (Show w, MonadWriter w m) => w -> m ()
          -- debugTell x = trace (show x) (tell x)
          debugTell x = tell x

          has_inner :: Category -> Category -> CategoryContext Bool
          {- substitutive rules -}
          has_inner (Composite _ [category]) small_category = do
            debugTell [Has{has_msg=UnwrapComposite,on_which=Big,big=big_category,small=small_category}]
            has category small_category
          has_inner big_category (Composite _ [category]) = do
            debugTell [Has{has_msg=UnwrapComposite,on_which=Small,big=big_category,small=small_category}]
            has big_category category
          has_inner def@Definition{def_category=d} other = do
            debugTell [Has{has_msg=GoInsideDefinition,on_which=Big,big=big_category,small=small_category}]
            has d other
          has_inner other def@Definition{def_category=d} = do
            debugTell [Has{has_msg=GoInsideDefinition,on_which=Small,big=big_category,small=small_category}]
            has other d
          has_inner mem@TypeAnnotation{big_category=bc, small_category=sc} other = do
            debugTell [Has{has_msg=UseTypeAnnotation,on_which=Big,big=big_category,small=small_category}]
            has bc other
          has_inner other mem@TypeAnnotation{big_category=bc, small_category=sc} = do
            debugTell [Has{has_msg=UseTypeAnnotation,on_which=Small,big=big_category,small=small_category}]
            has other bc
          has_inner (Placeholder _ Element category) small_category = do
            debugTell [Has{has_msg=UsePlaceholderCategory,on_which=Big,big=big_category,small=small_category}]
            has category small_category
          has_inner big_category (Placeholder _ Element category) = do
            debugTell [Has{has_msg=UsePlaceholderCategory,on_which=Small,big=big_category,small=small_category}]
            has big_category category
          has_inner (Placeholder _ Label category) small_category = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Big,big=big_category,small=small_category}]
            if isRecursiveCategory big_category
              then has (unroll Recursive big_category) small_category
              else has category small_category
          has_inner big_category (Placeholder _ Label category) = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Small,big=big_category,small=small_category}]
            if isRecursiveCategory small_category
              then has big_category (unroll Recursive big_category)
              else has category small_category
          has_inner (Placeholder _ Resolved category) small_category = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Big,big=big_category,small=small_category}]
            category `has` small_category
          has_inner big_category (Placeholder _ Resolved category) = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Small,big=big_category,small=small_category}]
            big_category `has` category
          has_inner c_big@Composite{composite_type=Composition} other_category = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Big,big=big_category,small=small_category}]
            cat <- flatten c_big
            cat `has` other_category
          has_inner other_category c_small@Composite{composite_type=Composition} = do
            debugTell [Has{has_msg=UnrollLabel,on_which=Small,big=big_category,small=small_category}]
            cat <- flatten c_small
            other_category `has` cat
          has_inner c1@Composite{composite_type=Function, inner_categories=Definition d:rest1} c2@Composite{composite_type=c2_type} = do
            debugTell [Has{has_msg=ReplaceInnerDefinitions,on_which=Big,big=big_category,small=small_category}]
            result <- call d c1
            result `has` c2
          has_inner c1@Composite{composite_type=Function} c2@Composite{composite_type=c2_type, inner_categories=Definition d:rest} = do
            debugTell [Has{has_msg=ReplaceInnerDefinitions,on_which=Small,big=big_category,small=small_category}]
            result <- call d c2
            c1 `has` result
          {- Composite Categories -}
          has_inner (Composite big_type []) (Composite small_type []) = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return $ big_type == small_type
          has_inner (Composite big_type []) _ = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Composite Tuple -}
          has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Tuple small_inner)
              | length big_inner /= length small_inner = do
                debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return False
              | otherwise = do
                debugTell [Has{has_msg=MapEvaluateInner,on_which=Both,big=big_category,small=small_category}]
                and <$> zipWithM has big_inner small_inner
          has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Union small_inner) = do
            debugTell [Has{has_msg=MapEvaluateInner,on_which=Small,big=big_category,small=small_category}]
            or <$> mapM (has c_big) small_inner
          has_inner c_big@(Composite Tuple _) other = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Composite Union -}
          has_inner other_category (Composite Union sum_inner) = do
            debugTell [Has{has_msg=MapEvaluateInner,on_which=Small,big=big_category,small=small_category}]
            and <$> mapM (has other_category) sum_inner
          has_inner (Composite Union sum_inner) other_category = do
            debugTell [Has{has_msg=MapEvaluateInner,on_which=Big,big=big_category,small=small_category}]
            or <$> mapM (`has` other_category) sum_inner
          {- Composite Match -}
          has_inner big@(Composite Match case_inner) other_category = do
            debugTell [Has{has_msg=Transform,on_which=Big,big=big_category,small=small_category}]
            has_inner big{composite_type=Union} other_category
          {- Composite Function -}
          has_inner c1@Composite{composite_type=Function, inner_categories=input1:rest1} c2@Composite{composite_type=c2_type}
              | isDataCompositeType c2_type = do
                debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return False
              | otherwise = do
                debugTell [Has{has_msg=Transform,on_which=Both,big=big_category,small=small_category}]
                has c1{composite_type=Tuple} c2{composite_type=Tuple}
          has_inner c1@Composite{composite_type=Function, inner_categories=input1:rest1} other = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Refined -}
          {- TODO: Handle refinements via SMT solver and approximate methods -}
          has_inner Refined{base=base, predicate=p} Refined{base=base_other, predicate=p_other} = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            and <$> sequence [has base p, has p p_other]
          has_inner Refined{base=base, predicate=p} other = do
              debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
              if base == other
                then return True
                else return False
          {- BuiltIn -}
          has_inner BuiltIn{} _ = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return True
          has_inner _ BuiltIn{special_type=Flexible} = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return True
          has_inner _ BuiltIn{special_type=Universal} = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False
          {- Reference -}
          has_inner r@Reference{} other = throwError [Error{error_type=UnresolvedReference, error_stack=[r]}]
          has_inner other r@Reference{} = throwError [Error{error_type=UnresolvedReference, error_stack=[r]}]
          {- Function Call -}
          has_inner bmc@FunctionCall{base=bbm, argument=ba} smc@FunctionCall{base=sbm, argument=sa} = do
            debugTell [Has{has_msg=MapEvaluateInner,on_which=Both,big=big_category,small=small_category}]
            result <- and <$> sequence [bbm `has` sbm, ba `has` sa]
            if result
              then do
                debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
                return result
              else do
                debugTell [Has{has_msg=Evaluate,on_which=Both,big=big_category,small=small_category}]
                bc <- call ba bbm
                sc <- call sa sbm
                bc `has` sc
          has_inner mc@FunctionCall{base=bm, argument=a} other = do
            debugTell [Has{has_msg=Evaluate,on_which=Big,big=big_category,small=small_category}]
            cat <- call a bm
            has cat other
          has_inner other mc@FunctionCall{base=bm, argument=a} = do
            debugTell [Has{has_msg=Evaluate,on_which=Small,big=big_category,small=small_category}]
            cat <- call a bm
            has other cat
          {- Access -}
          has_inner a@Access{base=b, access_id=id} other = do
            debugTell [Has{has_msg=Evaluate,on_which=Big,big=big_category,small=small_category}]
            cat <- evaluateAccess a
            has cat other
          {- Import -}
          has_inner i@Import{} i2@Import{} = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return (i == i2)
          has_inner other i@Import{} = throwError [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
          has_inner i@Import{} other = throwError [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
          {- Things -}
          -- equal things and other cases are handled above
          has_inner (Thing t) _ = do
            debugTell [Has{has_msg=HasResult,on_which=Both,big=big_category,small=small_category}]
            return False

{- This does not do any checking which requires evaluation -}
validateCategory :: Category -> CategoryContext Category
validateCategory t@(Thing Unnamed) = throwError [Error{error_type=UnnamedCategory, error_stack=[t]}]
validateCategory t@(Thing (Index _)) = throwError [Error{error_type=IndexedNamed, error_stack=[t]}]
validateCategory c@Composite{composite_type=composition_type, inner_categories=[]}
    | isFunctionCompositeType composition_type =
        throwError [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
    | otherwise = return c
validateCategory c@Composite{composite_type=composition_type, inner_categories=inner_cats}
    | (composition_type == Composition || composition_type == Match ) && not (all isFunctionComposite inner_cats) =
        throwError [Error{error_type=DataInFunctionComposite, error_stack=[c]}]
    | (composition_type == Function) && all (\x -> isImport x || isDefinition x) inner_cats =
        throwError [Error{error_type=InsufficientFunctionTerms, error_stack=[c]}]
    | otherwise = return c
validateCategory ph@Placeholder{name=Unnamed} = throwError [Error{error_type=UnnamedCategory, error_stack=[ph]}]
validateCategory ph@Placeholder{name=(Index _)} = throwError [Error{error_type=IndexedNamed, error_stack=[ph]}]
{- TODO: missed case here of nested function calls -}
validateCategory a@Access{base=bc, access_id=Unnamed} = throwError [Error{error_type=EmptyAccessID, error_stack=[a]}]
validateCategory other = return other

validateCategoryRecursive :: Category -> CategoryContext Category
validateCategoryRecursive = transformAST (Recurse . validateCategory)

-- simplifyInner :: Category -> Category
-- simplifyInner (Composite _ [any]) = any
-- simplifyInner ph@Placeholder{placeholder_type=Element, placeholder_category=category}
--     | level category == return (Specific 0) = category
--     | otherwise = ph
-- simplifyInner Placeholder{name=n, placeholder_type=Label, placeholder_category=p2@Placeholder{name=n2}} = p2
-- simplifyInner input_category = input_category

-- simplify :: Category -> Either [Error] Category
-- simplify = applyOnAST (const Nothing) (Right . simplifyInner)

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
                    map (\(ref, path) -> Placeholder (Name ref) CategoryData.Label ((Import . Reference . Name) path)) zipped_dir
                }
        else throwError [Error BadImport [Reference (Name fp)]]

evaluateImport :: CategoryImporter -> Category -> CategoryContextT IO Category
evaluateImport importer (Import i@(Import _)) = evaluateImport importer i
evaluateImport importer (Import (Reference (Name n))) = importer n
evaluateImport importer (Import a@Access{base=bc, access_id=id}) =
    let
        extractFilePath :: Category -> FilePath
        extractFilePath (Reference (Name n)) = n
        extractFilePath (Access bc (Name n)) = extractFilePath bc ++ "." ++ n
        extractFilePath _ = error "Something weird"
    in
        importer (extractFilePath a)
evaluateImport importer (Import c@(Composite Tuple inner)) = do
    result <- mapM (evaluateImport importer . Import) inner
    return c{inner_categories=result}
evaluateImport importer (Import p@Placeholder{name=n, placeholder_type=CategoryData.Label, placeholder_category=pc}) = do
    result <- evaluateImport importer (Import pc)
    case result of
        p@Placeholder{placeholder_type=label, placeholder_category=ph_c} -> return $ p{name=n}
        _ -> return $ p{placeholder_category=result}
evaluateImport importer something = error $ "what is this import case? " ++ show something

step :: CategoryEvalOptions -> Category -> CategoryContextT IO Category
step _ c@Composite{inner_categories=[something]} = do
  tell [Step{msg=SingleCompositeUnwrap, input=[c]}]
  return something
step _ def@Definition{def_category=d} = do
  tell [Step{msg=SingleDefUnwrap, input=[def]}]
  return d
step opts f@FunctionCall{base=bc, argument=a} =
    do
      tell [Step{msg=Calling, input=[f]}]
      identityToIO $ call a bc
    `catchError` \error -> do
      tell [Step{msg=DeeperEval, input=[f]}]
      stepped_bc <- step opts bc
      stepped_a <- step opts{reduce_composite=True} a
      let step_result = FunctionCall stepped_bc stepped_a
      if stepped_bc == bc && stepped_a == a
        then throwError error
        else return step_result
step opts@Options{importer=importer} i@Import{import_category=ic} =
  do
    tell [Step{msg=Importing, input=[i]}]
    evaluateImport importer i
  `catchError` \error -> do
    tell [Step{msg=DeeperEval, input=[i]}]
    result_ic <- step opts ic
    if result_ic == ic
        then throwError error
        else return $ Import result_ic
step opts mem@TypeAnnotation{big_category=bc, small_category=sc} = do
  tell [Step{msg=TypeAnnotationCheck, input=[mem]}]
  result <- identityToIO $ has bc sc
  if result
    then return sc
    else do
      tell [Step{msg=DeeperEval, input=[mem]}]
      stepped_bc <- step opts bc
      stepped_sc <- step opts sc
      let step_result = TypeAnnotation stepped_bc stepped_sc
      if stepped_bc == bc && stepped_sc == sc
        then throwError [Error{error_type=BadTypeAnnotation, error_stack=[mem]}]
        else return step_result
step opts c@Composite{composite_type=Function, inner_categories=(i@(Import something):rest)} = do
    tell [Step{msg=DeeperEval, input=[c]}]
    resolved_import <- step opts i
    let result = c{inner_categories=Definition resolved_import:rest}
    return result
step opts c@Composite{composite_type=Function, inner_categories=def@(Definition p@(Placeholder ph_name Label ph_c)):rest} = do
    tell [Step{msg=ApplyingDefinition, input=[c]}]
    let replacement = if isRecursiveCategory p then p else ph_c
    case rest of
      [] -> throwError [Error InsufficientFunctionTerms [c]]
      more_terms -> do
        let result = c{inner_categories=map (replaceReferences ph_name replacement) more_terms}
        return result
{- This skips a definition which has no label, which can't be referenced later -}
step opts c@Composite{composite_type=Function, inner_categories=Definition d:rest} = do
  tell [Step{msg=SkippingDefinition, input=[c]}]
  case rest of
      [] -> throwError [Error InsufficientFunctionTerms [c]]
      more_terms -> do
        let result = c{inner_categories=rest}
        return result
step opts a@Access{base=b, access_id=a_id} = do
    tell [Step{msg=Accessing, input=[a]}]
    identityToIO $ evaluateAccess a
  `catchError` \errors -> do
        tell [Step{msg=DeeperEval, input=[a]}]
        stepped_b <- step opts b
        if stepped_b == b
          then throwError errors
          else do
            let result = Access stepped_b a_id
            return result
step opts rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} = do
  tell [Step{msg=Simplifying, input=[rr]}]
  let result = ph_c
  return result
step opts@Options{reduce_composite=reduce_composite} c@Composite{composite_type=c_type, inner_categories=inner_c} =
    if not reduce_composite || isFunctionCompositeType c_type
        then do
          tell [Step{msg=Returning, input=[c]}]
          return c
        else do
          tell [Step ReducingComposite [c]]
          stepped_inner <- mapM (step opts) inner_c
          return $ Composite c_type stepped_inner
step _ other = do
  tell [Step{msg=Returning, input=[other]}]
  return other

multipleStepEvaluate :: Int -> CategoryEvalOptions -> Category -> CategoryContextT IO Category
multipleStepEvaluate 0 opts cat = do
  tell [Step{msg=Returning, input=[cat]}]
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
