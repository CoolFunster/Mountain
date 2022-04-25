{-# LANGUAGE LambdaCase #-}
module CategoryData where

import Data.List ( intercalate, find, sort )
import Data.Either
import Data.Dynamic
import Data.Typeable

import Debug.Trace (trace)
import Control.Monad
import GHC.Base (Monad, returnIO)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath.Posix (takeBaseName)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Bifunctor

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

data TracedCategory =
    Traced {
        logs :: LogMsgType,
        input :: [TracedCategory],
        output :: Errorable Category
    } |
    Simple (Errorable Category)
    deriving (Eq, Show, Read)

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

data Errorable a =
    Valid a |
    ErrorList {
        errors::[Error]
    }
    deriving (Eq, Show, Read)

instance Functor Errorable where
    fmap foo (Valid x) = Valid $ foo x
    fmap foo (ErrorList list) = ErrorList list

instance Applicative Errorable where
    pure x = Valid x
    Valid foo <*> any = fmap foo any
    ErrorList el <*> any = ErrorList el

instance Monad Errorable where
    Valid a >>= foo = foo a
    ErrorList error_list >>= foo = ErrorList error_list

newtype ErrorableT m a = ErrorableT { runErrorableT :: m (Errorable a)}
instance Monad m => Monad (ErrorableT m) where
    return = ErrorableT . return . Valid
    x >>= f = ErrorableT $ do
        errorable_value <- runErrorableT x
        case errorable_value of
            ErrorList ers  -> return (ErrorList ers)
            Valid value -> runErrorableT $ f value

instance Monad m => Applicative (ErrorableT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (ErrorableT m) where
    fmap = liftM

isValid :: Errorable a -> Bool
isValid (Valid _) = True
isValid _ = False

isError :: Errorable a -> Bool
isError (ErrorList _) = True
isError _ = False

partitionErrorable :: [Errorable a] -> ([a], [Error])
partitionErrorable = foldr (
    \new_errorable (l,r) ->
        case new_errorable of
            Valid something -> (something:l, r)
            ErrorList errors -> (l, errors ++ r)) ([],[])

addCategoryToErrorStack :: Category -> Error -> Error
addCategoryToErrorStack input_a e@Error{error_stack=stack} = e{error_stack=stack++[input_a]}

fromValid :: Errorable a -> a
fromValid (Valid x) = x
fromValid (ErrorList e) = error $ "bad fromValid " ++ show e

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

call :: Category -> Category -> Errorable Category
call arg p@Placeholder{placeholder_type=Label} = call arg (unroll Recursive p)
call arg c@Composite{composite_type=c_type,inner_categories=[]}
    | isDataCompositeType c_type = ErrorList [Error{error_type=CallingADataCompositeCategory, error_stack=[c]}]
    | otherwise = ErrorList [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
call arg c@Composite{inner_categories=[something]} = call arg something
call arg c@Composite{composite_type=Function, inner_categories=head:tail} =
    case head `tracedHas` arg of
        ErrorList errors -> ErrorList errors
        Valid False -> ErrorList [Error{error_type=InvalidArgument, error_stack=[arg, c]}]
        Valid True -> do
            let new_tail = case head of
                    p@Placeholder{name=id} -> map (replaceReferences id arg) tail
                    _ -> tail
            case new_tail of
                [something] -> Valid something
                [] -> ErrorList [Error{error_type=InsufficientFunctionTerms, error_stack=[arg, c]}]
                other -> Valid c{inner_categories=new_tail}
call arg c@Composite{composite_type=Case, inner_categories=inner} = do
    case find isValid (map (call arg) inner) of
        Just value -> value
        Nothing -> ErrorList [Error{error_type=BadFunctionCallInCase, error_stack=[arg, c]}]
call arg c@Composite{composite_type=Composition, inner_categories=inner} =
    foldl (\next_arg cat ->
        case next_arg of
            Valid narg -> call narg cat
            other -> other
    ) (Valid arg) inner
call arg c = ErrorList [Error{error_type=BadFunctionCall, error_stack=[arg, c]}]

compositeToFlatFunction :: Category -> Errorable Category
compositeToFlatFunction c@Composite{composite_type=Composition, inner_categories=head@Composite{composite_type=Function, inner_categories=inner_head:inner_rest}:rest} =
    case call inner_head c of
        ErrorList errors -> ErrorList errors
        Valid cat -> Valid c{composite_type=Function,inner_categories=[inner_head, cat]}
compositeToFlatFunction c@Composite{composite_type=Function, inner_categories=inner} = Valid c
compositeToFlatFunction c@Composite{composite_type=Case, inner_categories=inner} =
    case partitionErrorable $ map compositeToFlatFunction inner of
        (valid_categories, []) -> Valid $ Composite{composite_type=Function, inner_categories=[
            Composite{composite_type=Union, inner_categories=map (head . inner_categories) valid_categories},
            Composite{composite_type=Union, inner_categories=concatMap (tail . inner_categories) valid_categories}
        ]}
        (_, some_errors) -> ErrorList (map (addCategoryToErrorStack c) some_errors)
compositeToFlatFunction other = error $ "bad argument to uncheckedCompositeToFunction: " ++ show other

evaluateAccess :: Category -> Errorable Category
evaluateAccess a@Access{base=Composite{inner_categories=[]}, access_id=id} =
    ErrorList [Error{error_type=EmptyAccessBase,error_stack=[a]}]
evaluateAccess a@Access{base=Composite{inner_categories=[something]}} = evaluateAccess a{base=something}
evaluateAccess a@Access{base=c@Composite{composite_type=c_type, inner_categories=inner}, access_id=id} =
    case id of
        Index idx ->
            if isFunctionCompositeType c_type
                then ErrorList [Error{error_type=IndexAccessOnFunction, error_stack=[a]}]
                else Valid (inner!!idx)
        Name str_name -> do
            case filter (\x -> getName x == Just (Name str_name))  inner of
                [] -> ErrorList [Error{error_type=BadAccess,error_stack=[a]}]
                stuff -> do
                    let result = head stuff
                    if isRecursiveCategory result
                        then return result
                        else if isPlaceholder result
                            then return $ placeholder_category result
                            else return result
        Unnamed -> ErrorList [Error{error_type=EmptyAccessID,error_stack=[a]}]
evaluateAccess a@Access{base=Special{special_type=Flexible}, access_id=output} = Valid Special{special_type=Flexible}
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Label, placeholder_category=ph_c}, access_id=id} = do
    let new_base = if isRecursiveCategory p then unroll Recursive p else ph_c
    case evaluateAccess a{base=new_base} of
        ErrorList errors -> ErrorList (map (addCategoryToErrorStack a) errors)
        Valid cat -> Valid cat
evaluateAccess a@Access{base=p@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c}, access_id=id} = do
    case evaluateAccess a{base=ph_c} of
        ErrorList errors -> ErrorList (map (addCategoryToErrorStack a) errors)
        Valid cat -> Valid cat

-- evaluateAccess a@Access{base=Refined{base=b_cat, predicate=p}, access_id=id} =
--         {- TODO: handle predicates -}
--         ErrorList [Error RefinementNotHandledInAccess [a]]
-- evaluateAccess a@Access{base=f@FunctionCall{base=b, argument=arg}, access_id=output} =
--     case call arg b of
--         ErrorList errors -> ErrorList errors
--         Valid cat -> evaluateAccess a{base=cat}
-- evaluateAccess a@Access{base=a_inner@Access{}, access_id=id} =
--     case evaluateAccess a_inner of
--         ErrorList errors -> ErrorList (map (addCategoryToErrorStack a) errors)
--         Valid cat -> evaluateAccess a{base=cat}
-- evaluateAccess a@Access{base=Reference{}, access_id=id} = Valid (Special{special_type=Flexible})
evaluateAccess a@Access{} = ErrorList [Error{error_type=UndefinedAccess,error_stack=[a]}]
evaluateAccess other = error $ "Cannot evaluate access on non access category : " ++ show other

level :: Category -> Either CategoryLevel [Error]
level input_category =
    let
        levelOfCategoryList :: [Category] -> Either CategoryLevel [Error]
        levelOfCategoryList [] = Left $ Specific 0
        levelOfCategoryList inner_categories =
            case partitionEithers (map level inner_categories) of
                (just_valid_levels, []) ->
                    case filter (AnyLevel /=) just_valid_levels of
                        [] -> Left AnyLevel
                        other_levels -> Left $ maximum other_levels
                (_, some_errors) -> Right $ concat some_errors

        incrementLevel :: CategoryLevel -> CategoryLevel
        incrementLevel some_level =
            case some_level of
                Specific l -> Specific (1 + l)
                anything_else -> anything_else

        level_inner :: Category -> Either CategoryLevel [Error]
        level_inner input_category =
            case input_category of
                (Thing t) -> Left $ Specific 0
                (Composite _ inner) -> levelOfCategoryList inner
                p@Placeholder{placeholder_type=Element, placeholder_category=ph_c} ->
                    case level ph_c of
                        Right errors -> Right $ map (addCategoryToErrorStack p) errors
                        Left (Specific l) -> Left (LessThan l)
                        Left (LessThan l) -> Left $ LessThan l
                        Left Infinite -> Left AnyLevel
                        Left AnyLevel -> Left AnyLevel
                l@Placeholder{placeholder_type=Label, placeholder_category=ph_c} ->
                    let
                        category_of_interest =
                            if isRecursiveCategory ph_c
                                then unroll Flat l
                                else ph_c
                    in
                        case level category_of_interest of
                            Left level -> Left level
                            Right errors -> Right $ map (addCategoryToErrorStack l) errors
                rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} -> level ph_c
                Refined {base=_base_category, predicate=_predicate} -> level _base_category
                Special{special_type=Flexible} -> Left AnyLevel
                Special{special_type=Universal} -> Left Infinite
                Reference{} -> Left AnyLevel
                FunctionCall{base=b, argument=a} ->
                    case call a b of
                        ErrorList errors -> Right errors
                        Valid cat -> level cat
                a@Access{base=bc,access_id=id} ->
                    case evaluateAccess a of
                        Valid cat -> level cat
                        ErrorList errors -> Right (map (addCategoryToErrorStack a) errors)
                i@Import{} -> Left AnyLevel
                def@Definition{def_category=d} -> level d
                mem@Membership{big_category=bc, small_category=sc} -> level sc
    in
        if isRecursiveCategory input_category || isCompositeOfType Union input_category
            then case level_inner input_category of
                Left level -> Left (incrementLevel level)
                Right errors -> Right errors
            else level_inner input_category

tracedHas :: Category -> Category -> Errorable Bool
-- tracedHas a b = trace ("big: " ++ show a ++ "\nsmall: " ++ show b ++ "\n") (has a b)
tracedHas = has

has :: Category -> Category -> Errorable Bool
has big_category small_category
    | big_category == small_category = return True
    | otherwise = has_inner big_category small_category
        where
            has_inner :: Category -> Category -> Errorable Bool
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
                    ErrorList errors -> ErrorList errors
                    Valid cat -> tracedHas cat other_category
            has_inner other_category c_small@Composite{composite_type=Composition} =
                case compositeToFlatFunction c_small of
                    ErrorList errors -> ErrorList errors
                    Valid cat -> tracedHas cat other_category
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
            has_inner r@Reference{} other = ErrorList [Error{error_type=UnresolvedReference, error_stack=[r]}]
            has_inner other r@Reference{} = ErrorList [Error{error_type=UnresolvedReference, error_stack=[r]}]
            {- Function Call -}
            has_inner mc@FunctionCall{base=bm, argument=a} other =
                case call a bm of
                    ErrorList some_errors -> ErrorList some_errors
                    Valid some_cat -> tracedHas some_cat other
            has_inner other mc@FunctionCall{base=bm, argument=a} =
                case call a bm of
                    ErrorList some_errors -> ErrorList some_errors
                    Valid some_cat -> tracedHas other some_cat
            {- Access -}
            has_inner a@Access{base=b, access_id=id} other =
                case evaluateAccess a of
                    ErrorList some_errors -> ErrorList some_errors
                    Valid cat -> tracedHas cat other
            {- Import -}
            has_inner i@Import{} i2@Import{} = return (i == i2)
            has_inner i@Import{} other = ErrorList [Error{error_type=CannotTypecheckRawImport, error_stack=[i]}]
            {- Things -}
            -- equal things and other cases are handled above
            has_inner (Thing t) _ = return False

validateCategoryInner :: Category -> Errorable Category
validateCategoryInner t@(Thing Unnamed) = ErrorList [Error{error_type=UnnamedCategory, error_stack=[t]}]
validateCategoryInner t@(Thing (Index _)) = ErrorList [Error{error_type=IndexedNamed, error_stack=[t]}]
validateCategoryInner c@Composite{composite_type=composition_type, inner_categories=[]}
    | isFunctionCompositeType composition_type =
        ErrorList [Error{error_type=EmptyFunctionalComposite, error_stack=[c]}]
    | otherwise = return c
validateCategoryInner c@Composite{composite_type=composition_type, inner_categories=inner_cats}
    | (composition_type == Composition || composition_type == Case ) && not (all isFunctionComposite inner_cats) =
        ErrorList [Error{error_type=DataInFunctionComposite, error_stack=[c]}]
    | (composition_type == Function) && all (\x -> isImport x || isDefinition x) inner_cats =
        ErrorList [Error{error_type=InsufficientFunctionTerms, error_stack=[c]}]
    | otherwise = return c
validateCategoryInner ph@Placeholder{name=Unnamed} = ErrorList [Error{error_type=UnnamedCategory, error_stack=[ph]}]
validateCategoryInner ph@Placeholder{name=(Index _)} = ErrorList [Error{error_type=IndexedNamed, error_stack=[ph]}]
validateCategoryInner r@Refined{base=cat,predicate=p}
    | not (isFunctionComposite p) = ErrorList [Error{error_type=PredicateHasNonFunctionArgument, error_stack=[r]}]
    | Composite{inner_categories=head:rest} <- cat, return True /= (head `has` cat) = ErrorList [Error{error_type=BadRefinementPredicateInput, error_stack=[r]}]
    | otherwise = return r
{- TODO: missed case here of nested function calls -}
validateCategoryInner f@FunctionCall{base=c@Composite{inner_categories=[something]}, argument=a} = do
    let result = validateCategoryInner f{base=something}
    if isValid result
        then return f
        else result
validateCategoryInner f@FunctionCall{base=c@Composite{composite_type=c_type, inner_categories=head:rest}, argument=a}
    | not (isFunctionCompositeType c_type) = ErrorList [Error{error_type=NonFunctioninFunctionCallBase, error_stack=[f]}]
    | otherwise = return f
validateCategoryInner f@FunctionCall{base=Thing{}} = ErrorList [Error{error_type=NonFunctioninFunctionCallBase, error_stack=[f]}]
validateCategoryInner a@Access{base=bc, access_id=Unnamed} = ErrorList [Error{error_type=EmptyAccessID, error_stack=[a]}]
validateCategoryInner other = return other

validateCategory :: Category -> Errorable Category
validateCategory = applyOnAST (const Nothing) validateCategoryInner

simplifyInner :: Category -> Category
simplifyInner (Composite _ [any]) = any
simplifyInner ph@Placeholder{placeholder_type=Element, placeholder_category=category}
    | level category == Left (Specific 0) = category
    | otherwise = ph
simplifyInner Placeholder{name=n, placeholder_type=Label, placeholder_category=p2@Placeholder{name=n2}} = p2
simplifyInner input_category = input_category

simplify :: Category -> Errorable Category
simplify = applyOnAST (const Nothing) (Valid . simplifyInner)

-- Basic Interpreter makers

loadModule :: FilePath -> String -> (FilePath -> ErrorableT IO Category) -> FilePath -> ErrorableT IO Category
loadModule module_base_path file_ext importer fp =
    let
        repl '.' = '/'
        repl c = c
        file_name = module_base_path ++ map repl fp

        removeFileExt :: FilePath -> FilePath
        removeFileExt str = if str == file_ext then "" else  head str:removeFileExt (tail str)
    in
        ErrorableT $ do
            file_exist <- doesFileExist (file_name ++ file_ext)
            dir_exist <- doesDirectoryExist file_name
            if file_exist
                then do
                    runErrorableT $ importer (file_name ++ file_ext)
            else if dir_exist
                then do
                    dirContents <- listDirectory file_name
                    let baseDirContents = sort $ map removeFileExt dirContents
                    let loadedDir = map ((fp ++ ".") ++) baseDirContents
                    let zipped_dir = zip baseDirContents loadedDir
                    return $ Valid $ Composite{composite_type=Tuple, inner_categories=
                        map (\(ref, path) -> Placeholder (Name ref) CategoryData.Label ((Import . Reference . Name) path)) zipped_dir
                    }
            else return $ ErrorList [Error BadImport [Reference (Name fp)]]

evaluateImport :: (FilePath -> ErrorableT IO Category) -> Category -> ErrorableT IO Category
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

stepEvaluate :: Bool -> (FilePath -> ErrorableT IO Category) -> Category -> IO TracedCategory
stepEvaluate reduce_composite importer c@Composite{inner_categories=[something]} = return $ Traced SingleCompositeUnwrap [Simple $ return c] (return something)
stepEvaluate reduce_composite importer def@Definition{def_category=d} = return $ Traced SingleDefUnwrap [Simple $ return def] (return d)
stepEvaluate reduce_composite importer f@FunctionCall{base=bc, argument=a} = do
    let tried_call = call a bc
    if isValid tried_call
        then return $ Traced Calling [Simple $ return f] tried_call
        else do
            result_bc <- stepEvaluate reduce_composite importer bc
            result_a <- stepEvaluate True importer a
            if output result_bc == return bc && output result_a == return a
                then return $ Traced Calling [Simple $ return f] tried_call
                else return $ Traced DeeperEval [result_bc, result_a] $ FunctionCall <$> output result_bc <*> output result_a
stepEvaluate reduce_composite importer i@Import{import_category=ic} = do
    result <- runErrorableT $ evaluateImport importer i
    let default_result = Traced Importing [Simple $ return i] result
    if isValid result
        then return default_result
        else do
            result_ic <- stepEvaluate reduce_composite importer ic
            if output result_ic == return ic
                then return default_result
                else return $ Traced DeeperEval [result_ic] $ Import <$> output result_ic
stepEvaluate reduce_composite importer mem@Membership{big_category=bc, small_category=sc} =
    let
        default_val = Traced MembershipCheck [Simple $ return mem] $ return sc
    in
        if return True == tracedHas bc sc
            then return default_val
            else do
                result_bc <- stepEvaluate reduce_composite importer bc
                result_sc <- stepEvaluate reduce_composite importer sc
                if output result_bc == return bc && output result_sc == return sc
                    then return default_val{output=ErrorList [Error{error_type=BadMembership, error_stack=[mem]}]}
                    else return $ Traced DeeperEval [result_bc, result_sc] $ Membership <$> output result_bc <*> output result_sc    
stepEvaluate reduce_composite importer c@Composite{composite_type=Function, inner_categories=(i@(Import something):rest)} = do
    result <- stepEvaluate reduce_composite importer i
    return $ Traced DeeperEval [Simple $ return c, result] $ do
        eval <- output result
        return c{inner_categories=Definition eval:rest}
stepEvaluate reduce_composite importer c@Composite{composite_type=Function, inner_categories=def@(Definition p@(Placeholder ph_name Label ph_c)):rest} = do
    let replacement = if isRecursiveCategory p then p else ph_c
    let default_val = Traced ApplyingDefinition [Simple $ return c]
    case map (replaceReferences ph_name replacement) rest of
        [] -> return $ default_val $ ErrorList [Error InsufficientFunctionTerms [c]]
        something_else -> return $ default_val $ Valid c{inner_categories=something_else}
stepEvaluate reduce_composite importer c@Composite{composite_type=Function, inner_categories=Definition d:rest} =
    return $ Traced SkippingDefinition [Simple $ return c] $ return c{inner_categories=rest}
stepEvaluate reduce_composite importer a@Access{base=b, access_id=a_id} = do
    let result = evaluateAccess a
    let default_val = Traced Accessing [Simple $ return a]
    if isValid result
        then return $ default_val result
        else do
            result_b <- stepEvaluate reduce_composite importer b
            if output result_b == return b
                then return $ default_val result
                else return $ Traced DeeperEval [result_b] $ Access <$> output result_b <*> return a_id
stepEvaluate reduce_composite importer rr@Placeholder{placeholder_type=Resolved, placeholder_category=ph_c} =
    return $ Traced Simplifying [Simple $ return rr] $ return ph_c
stepEvaluate reduce_composite importer c@Composite{composite_type=c_type, inner_categories=inner_c} =
    if not reduce_composite || isFunctionCompositeType c_type
        then return $ Traced Returning [] $ Valid c
        else do
            result <- mapM (stepEvaluate reduce_composite importer) inner_c
            return $ Traced ReducingComposite result $ Composite c_type <$> mapM output result
stepEvaluate reduce_composite importer other = return $ Traced Returning [] $ Valid other

multipleStepEvaluate :: Int -> Bool -> (FilePath -> ErrorableT IO Category) -> Category -> IO [TracedCategory]
multipleStepEvaluate steps reduce_composite importer cat =
    if steps == 0
        then return [Traced Returning [] $ Valid cat]
        else do
            traced_new_cat <- stepEvaluate reduce_composite importer cat
            let new_cat = output traced_new_cat
            if isValid new_cat
                then do
                    result <- multipleStepEvaluate (steps-1) reduce_composite importer (fromValid new_cat)
                    return $ traced_new_cat:result
                else
                    return [traced_new_cat]

dbgEvaluate :: Bool -> (FilePath -> ErrorableT IO Category) -> Category -> IO [TracedCategory]
dbgEvaluate reduce_composite importer cat = do
    traced_new_cat <- stepEvaluate reduce_composite importer cat
    let new_cat = output traced_new_cat
    if isError new_cat || fromValid new_cat == cat
        then return [traced_new_cat]
        else do
            result <- dbgEvaluate reduce_composite importer (fromValid $ output traced_new_cat)
            return $ traced_new_cat:result

fullEvaluate :: Bool -> (FilePath -> ErrorableT IO Category) -> Category -> ErrorableT IO Category
fullEvaluate reduce_composite importer cat = ErrorableT $ do
    traced_new_cat <- stepEvaluate reduce_composite importer cat
    let new_cat = output traced_new_cat
    if isError new_cat || fromValid new_cat == cat
        then return new_cat
        else runErrorableT $ fullEvaluate reduce_composite importer (fromValid new_cat)

execute :: Bool -> (FilePath -> ErrorableT IO Category) -> Category -> ErrorableT IO Category
execute reduce_composite importer input_cat = do
    let step1 = simplify input_cat >>= validateCategory
    case step1 of
        ErrorList ers -> ErrorableT $ return $ ErrorList ers
        Valid cat -> fullEvaluate reduce_composite importer cat

dbgExecute :: Bool -> (FilePath -> ErrorableT IO Category) -> Category -> IO [TracedCategory]
dbgExecute reduce_composite importer input_cat = do
    let step1 = simplify input_cat >>= validateCategory
    case step1 of
        ErrorList ers -> return [Simple $ ErrorList ers]
        Valid cat -> dbgEvaluate reduce_composite importer cat
