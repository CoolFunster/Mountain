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

data SpecialCategoryType =
    Flexible |
    Universal
    deriving (Eq, Show, Read)

data CompositeType =
    Tuple | -- tuple
    Union | -- union
    Function | -- a function
    Composition | -- a chain of functions (a->b, b->c = a->c)
    Case -- a case statement of functions     (a->b, b->c = a->c)
    deriving (Eq, Show, Read)

data PlaceholderType =
    Label |
    Element |
    Resolved
    deriving (Eq, Show, Read)

data Category =
    -- categories
    Thing { name::Id } | -- a concrete object
    Composite {  -- a group of things
        composite_type::CompositeType,
        inner_categories::[Category]
    } |
    Placeholder {
        name::Id,
        placeholder_type::PlaceholderType,
        placeholder_category::Category
    } |
    Refined {
        base::Category,
        predicate::Category -- specifically predicates are morphisms of Category -> Bool
    } |
    Special {
        special_type::SpecialCategoryType
    } |
    Reference {
        name::Id
    } |
    -- language constructs
    FunctionCall {
        base::Category,
        argument::Category
    } |
    Access {
        base::Category,
        access_id::Id
    } |
    Import { -- TODO MAKE ON REFERENCE & TUPLE of REFS
        import_category::Category
    } |
    Definition {
        def_category::Category
    } |
    Membership {
        big_category::Category,
        small_category::Category
    }
    deriving (Eq, Show, Read)

data LogMsgType =
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
    MembershipCheck |
    Accessing
    deriving (Eq, Show, Read)

-- basic functions
data CategoryLog =
  Info {
    msg::LogMsgType,
    input :: [Category]
  } deriving (Show)

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
    BadFunctionCallInCase |
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
    BadMembership
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

type CategoryImporter = FilePath -> CategoryContextT IO Category

data CategoryEvalOptions = Options {
  reduce_composite :: Bool,
  importer :: CategoryImporter
}

-- useful categories

valid :: Category
valid = Composite Tuple []

empty :: Category
empty = Composite Union []

universal :: Category
universal = Special{special_type=Universal}

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
isDataCompositeType Union = True
isDataCompositeType _ = False

isFunctionCompositeType :: CompositeType -> Bool
isFunctionCompositeType = not . isDataCompositeType

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

isSpecial :: Category -> Bool
isSpecial Special{} = True
isSpecial _ = False

isSpecialType :: SpecialCategoryType -> Category -> Bool
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
checkAST aggregator checker s@Special{} = checker s
checkAST aggregator checker r@Reference{} = checker r
checkAST aggregator checker fc@FunctionCall{base=b, argument=a} = aggregator (checker fc: map (checkAST aggregator checker) [b,a])
checkAST aggregator checker a@Access{base=b, access_id=id} = aggregator [checker a, checkAST aggregator checker b]
checkAST aggregator checker i@Import{} = checker i
checkAST aggregator checker def@Definition{def_category=d} = aggregator [checker def, checkAST aggregator checker d]
checkAST aggregator checker mem@Membership{big_category=bc, small_category=sc} = aggregator (checker mem: map (checkAST aggregator checker) [bc,sc])

{- preapplicator goes first and applies if necessary, post applicator applies from bottom up -}
applyOnAST :: (Monad m) => (Category -> Maybe (m Category)) -> (Category -> m Category) -> Category -> m Category
applyOnAST preapplicator postapplicator category =
    case preapplicator category of
        Just something -> something
        Nothing -> applyOnASTInner category

    where
        recurse = applyOnAST preapplicator postapplicator

        applyOnASTInner t@Thing{} = postapplicator t
        applyOnASTInner c@(Composite c_type inner) = mapM recurse inner >>= postapplicator . Composite c_type
        applyOnASTInner p@(Placeholder n p_type ph_c) = recurse ph_c >>= postapplicator . Placeholder n p_type
        applyOnASTInner r@(Refined b p) = Refined <$> recurse b <*> recurse p >>= postapplicator
        applyOnASTInner r@(Reference n) = postapplicator r
        applyOnASTInner s@(Special t) = postapplicator s
        applyOnASTInner fc@(FunctionCall b a) = FunctionCall <$> recurse b <*> recurse a >>= postapplicator
        applyOnASTInner a@(Access b a_id) = Access <$> recurse b <*> return a_id >>= postapplicator
        applyOnASTInner i@(Import i_c) = Import <$> postapplicator i_c
        applyOnASTInner d@(Definition d_c) = recurse d_c >>= postapplicator . Definition
        applyOnASTInner m@(Membership b s) = Membership <$> recurse b <*> recurse s >>= postapplicator


-- {- Category Functions -}

isRecursiveCategory :: Category -> Bool
isRecursiveCategory Placeholder{name=ph_name, placeholder_type=Label, placeholder_category=ph_c} = checkAST or (isReferenceOfName ph_name) ph_c
isRecursiveCategory _ = False

replaceReferences ::  Id -> Category -> Category -> Category
replaceReferences ref_name new_cat base_cat =
    let
        preReplace :: (Category -> Maybe (Identity Category))
        preReplace p@Placeholder{name=ph_n, placeholder_type=Label}
            | ph_n == ref_name && isRecursiveCategory p = Just $ return p
            | otherwise = Nothing
        preReplace c = Nothing

        postReplace :: (Category -> Identity Category)
        postReplace r@Reference{name=n} = if n == ref_name then return (Placeholder n Resolved new_cat) else return r
        postReplace c = return c
    in
        runIdentity $ applyOnAST preReplace postReplace base_cat

replaceResolved :: Category -> Category
replaceResolved cat =
    let
        postReplace :: (Category -> Identity Category)
        postReplace rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} = return ph_c
        postReplace cat = return cat
    in
        runIdentity $ applyOnAST (const Nothing) postReplace cat

data UnrollType = Flat | Recursive
unroll :: UnrollType -> Category -> Category
unroll Flat l@Placeholder{name=rec_name, placeholder_type=Label, placeholder_category=rec_cat} =
    replaceReferences rec_name Special{special_type=Flexible} rec_cat
unroll Recursive l@Placeholder{name=rec_name, placeholder_type=Label, placeholder_category=rec_cat} =
    replaceReferences rec_name l rec_cat
unroll _ something_else = something_else

call :: Category -> Category -> Either [Error] Category
call arg p@Placeholder{placeholder_type=Label} = call arg (unroll Recursive p)
call arg c@Composite{composite_type=c_type,inner_categories=[]}
    | isDataCompositeType c_type = Left [Error{error_type=CallingADataCompositeCategory, error_stack=[c]}]
    | otherwise = Left [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
call arg c@Composite{inner_categories=[something]} = call arg something
call arg c@Composite{composite_type=Function, inner_categories=head:tail} =
    case head `tracedHas` arg of
        Left errors -> Left errors
        Right False -> Left [Error{error_type=InvalidArgument, error_stack=[arg, c]}]
        Right True -> do
            let new_tail = case head of
                    p@Placeholder{name=id} -> map (replaceReferences id arg) tail
                    _ -> tail
            case new_tail of
                [something] -> Right something
                [] -> Left [Error{error_type=InsufficientFunctionTerms, error_stack=[arg, c]}]
                other -> Right c{inner_categories=new_tail}
call arg c@Composite{composite_type=Case, inner_categories=inner} = do
    case find isRight (map (call arg) inner) of
        Just value -> value
        Nothing -> Left [Error{error_type=BadFunctionCallInCase, error_stack=[arg, c]}]
call arg c@Composite{composite_type=Composition, inner_categories=inner} =
    foldl (\next_arg cat ->
        case next_arg of
            Right narg -> call narg cat
            other -> other
    ) (Right arg) inner
call arg c = Left [Error{error_type=BadFunctionCall, error_stack=[arg, c]}]

compositeToFlatFunction :: Category -> Either [Error] Category
compositeToFlatFunction c@Composite{composite_type=Composition, inner_categories=head@Composite{composite_type=Function, inner_categories=inner_head:inner_rest}:rest} =
    case call inner_head c of
        Left errors -> Left errors
        Right cat -> Right c{composite_type=Function,inner_categories=[inner_head, cat]}
compositeToFlatFunction c@Composite{composite_type=Function, inner_categories=inner} = Right c
compositeToFlatFunction c@Composite{composite_type=Case, inner_categories=inner} =
    case partitionEithers $ map compositeToFlatFunction inner of
        ([], valid_categories) -> Right $ Composite{composite_type=Function, inner_categories=[
            Composite{composite_type=Union, inner_categories=map (head . inner_categories) valid_categories},
            Composite{composite_type=Union, inner_categories=concatMap (tail . inner_categories) valid_categories}
        ]}
        (some_errors, _) -> Left (map (addCategoryToErrorStack c) (concat some_errors))
compositeToFlatFunction other = error $ "bad argument to uncheckedCompositeToFunction: " ++ show other

evaluateAccess :: Category -> Either [Error] Category
evaluateAccess a@Access{base=Composite{inner_categories=[]}, access_id=id} =
    Left [Error{error_type=EmptyAccessBase,error_stack=[a]}]
evaluateAccess a@Access{base=Composite{inner_categories=[something]}} = evaluateAccess a{base=something}
evaluateAccess a@Access{base=c@Composite{composite_type=c_type, inner_categories=inner}, access_id=id} =
    case id of
        Index idx ->
            if isFunctionCompositeType c_type
                then Left [Error{error_type=IndexAccessOnFunction, error_stack=[a]}]
                else Right (inner!!idx)
        Name str_name -> do
            case filter (\x -> getName x == Just (Name str_name))  inner of
                [] -> Left [Error{error_type=BadAccess,error_stack=[a]}]
                stuff -> do
                    let result = head stuff
                    if isRecursiveCategory result
                        then return result
                        else if isPlaceholder result
                            then return $ placeholder_category result
                            else return result
        Unnamed -> Left [Error{error_type=EmptyAccessID,error_stack=[a]}]
evaluateAccess a@Access{base=Special{special_type=Flexible}, access_id=output} = Right Special{special_type=Flexible}
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Label, placeholder_category=ph_c}, access_id=id} = do
    let new_base = if isRecursiveCategory p then unroll Recursive p else ph_c
    case evaluateAccess a{base=new_base} of
        Left errors -> Left (map (addCategoryToErrorStack a) errors)
        Right cat -> Right cat
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c}, access_id=id} = do
    case evaluateAccess a{base=ph_c} of
        Left errors -> Left (map (addCategoryToErrorStack a) errors)
        Right cat -> Right cat
evaluateAccess a@Access{base=Refined{base=b_cat, predicate=p}, access_id=id} =
        {- TODO: handle predicates -}
        Left [Error RefinementNotHandledInAccess [a]]
evaluateAccess a@Access{base=f@FunctionCall{base=b, argument=arg}, access_id=output} =
    case call arg b of
        Left errors -> Left errors
        Right cat -> evaluateAccess a{base=cat}
evaluateAccess a@Access{base=a_inner@Access{}, access_id=id} =
    case evaluateAccess a_inner of
        Left errors -> Left (map (addCategoryToErrorStack a) errors)
        Right cat -> evaluateAccess a{base=cat}
evaluateAccess a@Access{base=Reference{}, access_id=id} = Right (Special{special_type=Flexible})
evaluateAccess a@Access{} = Left [Error{error_type=UndefinedAccess,error_stack=[a]}]
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

level :: Category -> Either [Error] CategoryLevel
level input_category =
    let
        levelOfCategoryList :: [Category] -> Either [Error] CategoryLevel
        levelOfCategoryList [] = Right $ Specific 0
        levelOfCategoryList inner_categories =
            case partitionEithers (map level inner_categories) of
                ([], just_valid_levels) ->
                    case filter (AnyLevel /=) just_valid_levels of
                        [] -> Right AnyLevel
                        other_levels -> Right $ maximum other_levels
                (some_errors, _) -> Left $ concat some_errors

        incrementLevel :: CategoryLevel -> CategoryLevel
        incrementLevel some_level =
            case some_level of
                Specific l -> Specific (1 + l)
                anything_else -> anything_else

        level_inner :: Category -> Either [Error] CategoryLevel
        level_inner input_category =
            case input_category of
                (Thing t) -> Right $ Specific 0
                (Composite _ inner) -> levelOfCategoryList inner
                p@Placeholder{placeholder_type=Element, placeholder_category=ph_c} ->
                    case level ph_c of
                        Left errors -> Left $ map (addCategoryToErrorStack p) errors
                        Right (Specific l) -> Right (LessThan l)
                        Right (LessThan l) -> Right $ LessThan l
                        Right Infinite -> Right AnyLevel
                        Right AnyLevel -> Right AnyLevel
                l@Placeholder{placeholder_type=Label, placeholder_category=ph_c} ->
                    let
                        category_of_interest =
                            if isRecursiveCategory ph_c
                                then unroll Flat l
                                else ph_c
                    in
                        case level category_of_interest of
                            Right level -> Right level
                            Left errors -> Left $ map (addCategoryToErrorStack l) errors
                rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} -> level ph_c
                Refined {base=_base_category, predicate=_predicate} -> level _base_category
                Special{special_type=Flexible} -> Right AnyLevel
                Special{special_type=Universal} -> Right Infinite
                Reference{} -> Right AnyLevel
                FunctionCall{base=b, argument=a} ->
                    case call a b of
                        Left errors -> Left errors
                        Right cat -> level cat
                a@Access{base=bc,access_id=id} ->
                    case evaluateAccess a of
                        Right cat -> level cat
                        Left errors -> Left (map (addCategoryToErrorStack a) errors)
                i@Import{} -> Right AnyLevel
                def@Definition{def_category=d} -> level d
                mem@Membership{big_category=bc, small_category=sc} -> level sc
    in
        if isRecursiveCategory input_category || isCompositeOfType Union input_category
            then case level_inner input_category of
                Right level -> Right (incrementLevel level)
                Left errors -> Left errors
            else level_inner input_category

tracedHas :: Category -> Category -> Either [Error] Bool
-- tracedHas a b = trace ("big: " ++ show a ++ "\nsmall: " ++ show b ++ "\n") (has a b)
tracedHas = has

has :: Category -> Category -> Either [Error] Bool
has big_category small_category
    | big_category == small_category = return True
    | otherwise = has_inner big_category small_category
        where
            has_inner :: Category -> Category -> Either [Error] Bool
            {- substitutive rules -}
            has_inner (Composite _ [category]) small_category = tracedHas category small_category
            has_inner big_category (Composite _ [category]) = tracedHas big_category category
            has_inner def@Definition{def_category=d} other = tracedHas d other
            has_inner other def@Definition{def_category=d} = tracedHas other d
            has_inner mem@Membership{big_category=bc, small_category=sc} other = tracedHas sc other
            has_inner (Placeholder _ Element category) small_category
                | category == small_category = return True
                | otherwise = tracedHas category small_category
            has_inner big_category (Placeholder _ Element category) = tracedHas big_category category
            has_inner (Placeholder _ Label category) small_category
                | isRecursiveCategory big_category = tracedHas (unroll Recursive big_category) small_category
                | otherwise = tracedHas category small_category
            has_inner big_category (Placeholder _ Label category)
                | isRecursiveCategory small_category = tracedHas big_category (unroll Recursive big_category)
                | otherwise = tracedHas category small_category
            has_inner big_category (Placeholder _ Resolved category) = big_category `tracedHas` category
            has_inner (Placeholder _ Resolved category) small_category = category `tracedHas` small_category
            has_inner c_big@Composite{composite_type=Composition} other_category =
                case compositeToFlatFunction c_big of
                    Left errors -> Left errors
                    Right cat -> tracedHas cat other_category
            has_inner other_category c_small@Composite{composite_type=Composition} =
                case compositeToFlatFunction c_small of
                    Left errors -> Left errors
                    Right cat -> tracedHas cat other_category
            has_inner c1@Composite{composite_type=Function, inner_categories=Definition d:rest1} c2@Composite{composite_type=c2_type} = do
                result <- call d c1
                result `tracedHas` c2
            has_inner c1@Composite{composite_type=Function} c2@Composite{composite_type=c2_type, inner_categories=Definition d:rest} = do
                result <- call d c2
                c1 `tracedHas` result
            {- Composite Categories -}
            has_inner (Composite big_type []) (Composite small_type []) = return $ big_type == small_type
            has_inner (Composite big_type []) _ = return False
            {- Composite Tuple -}
            has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Tuple small_inner)
                | length big_inner /= length small_inner = return False
                | otherwise = return $ all (== return True) (zipWith tracedHas big_inner small_inner)
            has_inner c_big@(Composite Tuple big_inner) c_small@(Composite Union small_inner) =
                 return $ elem (return True) (map (tracedHas c_big) small_inner)
            has_inner c_big@(Composite Tuple _) other = return False
            {- Composite Union -}
            has_inner other_category (Composite Union sum_inner) =
                return $ all ((== return True) . tracedHas other_category) sum_inner
            has_inner (Composite Union sum_inner) other_category =
                return $ elem (return True) (map tracedHas sum_inner <*> [other_category])
            {- Composite Case -}
            has_inner big@(Composite Case case_inner) other_category = has_inner big{composite_type=Union} other_category
            {- Composite Function -}
            has_inner c1@Composite{composite_type=Function, inner_categories=input1:rest1} c2@Composite{composite_type=c2_type}
                | isDataCompositeType c2_type = return False
                | otherwise = tracedHas c1{composite_type=Tuple} c2{composite_type=Tuple}
            has_inner c1@Composite{composite_type=Function, inner_categories=input1:rest1} other = return False
            {- Refined -}
            {- TODO: Handle refinements via SMT solver and approximate methods -}
            has_inner Refined{base=base, predicate=p} Refined{base=base_other, predicate=p_other} =
                return $ all (== return True) [tracedHas base p, tracedHas p p_other]
            has_inner Refined{base=base, predicate=p} other
                | base == other = return True
                | otherwise = return False
            {- Special -}
            has_inner Special{} _ = return True
            has_inner _ Special{special_type=Flexible} = return True
            has_inner _ Special{special_type=Universal} = return False
            {- Reference -}
            has_inner r@Reference{} other = Left [Error{error_type=UnresolvedReference, error_stack=[r]}]
            has_inner other r@Reference{} = Left [Error{error_type=UnresolvedReference, error_stack=[r]}]
            {- Function Call -}
            has_inner mc@FunctionCall{base=bm, argument=a} other =
                case call a bm of
                    Left some_errors -> Left some_errors
                    Right some_cat -> tracedHas some_cat other
            has_inner other mc@FunctionCall{base=bm, argument=a} =
                case call a bm of
                    Left some_errors -> Left some_errors
                    Right some_cat -> tracedHas other some_cat
            {- Access -}
            has_inner a@Access{base=b, access_id=id} other =
                case evaluateAccess a of
                    Left some_errors -> Left some_errors
                    Right cat -> tracedHas cat other
            {- Import -}
            has_inner i@Import{} i2@Import{} = return (i == i2)
            has_inner i@Import{} other = Left [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
            {- Things -}
            -- equal things and other cases are handled above
            has_inner (Thing t) _ = return False

validateCategoryInner :: Category -> Either [Error] Category
validateCategoryInner t@(Thing Unnamed) = Left [Error{error_type=UnnamedCategory, error_stack=[t]}]
validateCategoryInner t@(Thing (Index _)) = Left [Error{error_type=IndexedNamed, error_stack=[t]}]
validateCategoryInner c@Composite{composite_type=composition_type, inner_categories=[]}
    | isFunctionCompositeType composition_type =
        Left [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
    | otherwise = return c
validateCategoryInner c@Composite{composite_type=composition_type, inner_categories=inner_cats}
    | (composition_type == Composition || composition_type == Case ) && not (all isFunctionComposite inner_cats) =
        Left [Error{error_type=DataInFunctionComposite, error_stack=[c]}]
    | (composition_type == Function) && all (\x -> isImport x || isDefinition x) inner_cats =
        Left [Error{error_type=InsufficientFunctionTerms, error_stack=[c]}]
    | otherwise = return c
validateCategoryInner ph@Placeholder{name=Unnamed} = Left [Error{error_type=UnnamedCategory, error_stack=[ph]}]
validateCategoryInner ph@Placeholder{name=(Index _)} = Left [Error{error_type=IndexedNamed, error_stack=[ph]}]
validateCategoryInner r@Refined{base=cat,predicate=p}
    | not (isFunctionComposite p) = Left [Error{error_type=PredicateHasNonFunctionArgument, error_stack=[r]}]
    | Composite{inner_categories=head:rest} <- cat, return True /= (head `has` cat) = Left [Error{error_type=BadRefinementPredicateInput, error_stack=[r]}]
    | otherwise = return r
{- TODO: missed case here of nested function calls -}
validateCategoryInner f@FunctionCall{base=c@Composite{inner_categories=[something]}, argument=a} = do
    let result = validateCategoryInner f{base=something}
    if isRight result
        then return f
        else result
validateCategoryInner f@FunctionCall{base=c@Composite{composite_type=c_type, inner_categories=head:rest}, argument=a}
    | not (isFunctionCompositeType c_type) = Left [Error{error_type=NonFunctioninFunctionCallBase, error_stack=[f]}]
    | otherwise = return f
validateCategoryInner f@FunctionCall{base=Thing{}} = Left [Error{error_type=NonFunctioninFunctionCallBase, error_stack=[f]}]
validateCategoryInner a@Access{base=bc, access_id=Unnamed} = Left [Error{error_type=EmptyAccessID, error_stack=[a]}]
validateCategoryInner other = return other

validateCategory :: Category -> Either [Error] Category
validateCategory = applyOnAST (const Nothing) validateCategoryInner

simplifyInner :: Category -> Category
simplifyInner (Composite _ [any]) = any
simplifyInner ph@Placeholder{placeholder_type=Element, placeholder_category=category}
    | level category == Right (Specific 0) = category
    | otherwise = ph
simplifyInner Placeholder{name=n, placeholder_type=Label, placeholder_category=p2@Placeholder{name=n2}} = p2
simplifyInner input_category = input_category

simplify :: Category -> Either [Error] Category
simplify = applyOnAST (const Nothing) (Right . simplifyInner)

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
  tell [Info{msg=SingleCompositeUnwrap, input=[c]}]
  return something
step _ def@Definition{def_category=d} = do
  tell [Info{msg=SingleDefUnwrap, input=[def]}]
  return d
step opts f@FunctionCall{base=bc, argument=a} =
    do
      tell [Info{msg=Calling, input=[f]}]
      case call a bc of
        Right result -> do
            return result
        Left errors -> do
            tell [Info{msg=DeeperEval, input=[f]}]
            stepped_bc <- step opts bc
            stepped_a <- step opts{reduce_composite=True} a
            let step_result = FunctionCall stepped_bc stepped_a
            if stepped_bc == bc && stepped_a == a
              then throwError errors
              else return step_result
step opts@Options{importer=importer} i@Import{import_category=ic} =
  do
    tell [Info{msg=Importing, input=[i]}]
    evaluateImport importer i
  `catchError` \error -> do
    tell [Info{msg=DeeperEval, input=[i]}]
    result_ic <- step opts ic
    if result_ic == ic
        then throwError error
        else return $ Import result_ic
step opts mem@Membership{big_category=bc, small_category=sc} = do
  tell [Info{msg=MembershipCheck, input=[mem]}]
  case tracedHas bc sc of
    Right True -> do
      return sc
    _ -> do
      tell [Info{msg=DeeperEval, input=[mem]}]
      stepped_bc <- step opts bc
      stepped_sc <- step opts sc
      let step_result = Membership stepped_bc stepped_sc
      if stepped_bc == bc && stepped_sc == sc
        then throwError [Error{error_type=BadMembership, error_stack=[mem]}]
        else return step_result
step opts c@Composite{composite_type=Function, inner_categories=(i@(Import something):rest)} = do
    tell [Info{msg=DeeperEval, input=[c]}]
    resolved_import <- step opts i
    let result = c{inner_categories=Definition resolved_import:rest}
    return result
step opts c@Composite{composite_type=Function, inner_categories=def@(Definition p@(Placeholder ph_name Label ph_c)):rest} = do
    tell [Info{msg=ApplyingDefinition, input=[c]}]
    let replacement = if isRecursiveCategory p then p else ph_c
    case rest of
      [] -> throwError [Error InsufficientFunctionTerms [c]]
      more_terms -> do
        let result = c{inner_categories=map (replaceReferences ph_name replacement) more_terms}
        return result
{- This skips a definition which has no label, which can't be referenced later -}
step opts c@Composite{composite_type=Function, inner_categories=Definition d:rest} = do
  tell [Info{msg=SkippingDefinition, input=[c]}]
  case rest of
      [] -> throwError [Error InsufficientFunctionTerms [c]]
      more_terms -> do
        let result = c{inner_categories=rest}
        return result
step opts a@Access{base=b, access_id=a_id} = do
    tell [Info{msg=Accessing, input=[a]}]
    case evaluateAccess a of
      Right result -> do
        return result
      Left errors -> do
        tell [Info{msg=DeeperEval, input=[a]}]
        stepped_b <- step opts b
        if stepped_b == b
          then throwError errors
          else do
            let result = Access stepped_b a_id
            return result
step opts rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} = do
  tell [Info{msg=Simplifying, input=[rr]}]
  let result = ph_c
  return result
step opts@Options{reduce_composite=reduce_composite} c@Composite{composite_type=c_type, inner_categories=inner_c} =
    if not reduce_composite || isFunctionCompositeType c_type
        then do
          tell [Info{msg=Returning, input=[c]}]
          return c
        else do
          tell [Info ReducingComposite [c]]
          stepped_inner <- mapM (step opts) inner_c
          return $ Composite c_type stepped_inner
step _ other = do
  tell [Info{msg=Returning, input=[other]}]
  return other

multipleStepEvaluate :: Int -> CategoryEvalOptions -> Category -> CategoryContextT IO Category
multipleStepEvaluate 0 opts cat = do
  tell [Info{msg=Returning, input=[cat]}]
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
