module CategoryCore where

import CategoryData
import CategorySynthesizer

import Debug.Trace
import Data.Monoid hiding (Sum, Product)
import Data.List (find)
import Data.Maybe (mapMaybe, fromJust, isNothing, isJust, catMaybes)

import FrontEnds.Textual.V1.CategoryWriter (categoryToStr)

{- Category Functions -}
equal :: Category -> Category -> Bool
equal a b = a == b || a `has` b && b `has` a

-- has (containing expr, inner expr)
has :: Category -> Category -> Bool
has big_category small_category
    | big_category == small_category = True
    | Special{} <- big_category = True
    | Special{} <- small_category = True
    | Reference{} <- big_category = True
    | Reference{} <- small_category = True
    | Label{target=t} <- big_category = CategoryCore.has t small_category
    | Label{target=t} <- small_category = CategoryCore.has big_category t
    | isJust (level big_category) && isJust (level small_category) && level big_category < level small_category = False
    | Placeholder{ph_level=ph_level, ph_category=ph_category} <- small_category = (ph_level <= level big_category) && CategoryCore.has big_category ph_category
    | (Composite _ [category]) <- big_category = CategoryCore.has category small_category
    | (Composite _ [category]) <- small_category = CategoryCore.has big_category category
    | im@IntermediateMorphism{} <- big_category = has (asMorphism im) small_category
    | im@IntermediateMorphism{} <- small_category = has big_category (asMorphism im)
    | otherwise = has_inner big_category small_category
        where
            {- Things -}
            -- equality is checked above only here if 2 things not eq
            has_inner (Thing t) _ = False
            {- Morphisms -}
            has_inner Morphism{input=input1,output=output1} Morphism{input=input2,output=output2} = CategoryCore.has input1 input2 && CategoryCore.has output1 output2
            has_inner m_big@Morphism{} c_small@Composite{} = CategoryCore.has m_big (asMorphism c_small)
            has_inner Morphism{} _ = False
            {- Composite Categories -}
            has_inner (Composite big_type []) (Composite small_type []) = big_type == small_type
            has_inner (Composite big_type []) _ = False
            has_inner (Composite Product product_inner) other_category
                | not (isCompositeType Product other_category) = False
                | isCompositeType Product other_category && length product_inner /= length (inner other_category) = False
                | otherwise = and $ zipWith CategoryCore.has product_inner (inner other_category)
            has_inner (Composite Sum sum_inner) other_category@(Composite Sum _) = all (CategoryCore.has other_category) sum_inner
            has_inner (Composite Sum sum_inner) other_category = or $ map CategoryCore.has sum_inner <*> [other_category]
            {- Higher Categories -}
            has_inner h@Composite{composition_type=Higher} other_category = isAnswered $ synthesizeCategory h other_category
            has_inner big_category h@Composite{composition_type=Higher,inner=inner_terms} = all (has_inner big_category) inner_terms
            {- Composite Morphism -}
            has_inner c_big@Composite{composition_type=Composition} other_category = CategoryCore.has (asMorphism c_big) other_category
            has_inner (Composite Sumposition sump_inner) other_category = or $ map CategoryCore.has sump_inner <*> [other_category]
            {- Placeholders -}
            has_inner Placeholder{ph_level=ph_level,ph_category=ph_category} other_category =
                case (ph_level, level other_category) of
                    (Just level, Just other_level) -> level <= other_level && CategoryCore.has other_category ph_category
                    _ -> error "Bad leveling!"
            has_inner mc@MorphismCall{base_morphism=bm, argument=a} other =
                let
                    call_result = call bm a
                in
                    case call_result of
                        Nothing -> False
                        _ -> has (fromJust call_result) other
            has_inner big small = error $ "has not implemented yet for " ++ show big ++ " has " ++ show small

isValidCategory :: Category -> Bool
isValidCategory any_category = isNothing $ validCategory any_category

_concatenateErrors :: [Maybe [[Char]]] -> Maybe [[Char]]
_concatenateErrors input_list = do
    let result_list = catMaybes input_list
    let concat_result_list = concat result_list
    case concat_result_list of
        [] -> Nothing
        _ -> Just concat_result_list

_makeErrorMsg :: Bool -> [Char] -> Maybe [[Char]]
_makeErrorMsg check error_msg = if check then Just [error_msg] else Nothing

validCategory :: Category -> Maybe [[Char]]
validCategory (Thing _) = Nothing
validCategory Morphism{input=m_input, output=m_output} = _concatenateErrors [validCategory m_input, validCategory m_output]
validCategory c@(Composite comp_type inner) = do
    let bad_null_inner = _makeErrorMsg (comp_type `elem` [Sumposition, Composition, Higher] && null inner) (show comp_type ++ " must contain function values. Given empty inner in " ++ show c)
    let all_is_morphic = _makeErrorMsg ((comp_type `elem` [Sumposition, Composition]) && not (all isMorphic inner)) ("Not all elements are morphic in " ++ show c)
    _concatenateErrors $ [bad_null_inner, all_is_morphic] ++ map validCategory inner
validCategory ph@(Placeholder _ (Just ph_level) ph_category) = do
    let ph_level_error = if ph_level < 0 then Just ["Ph level is less than zero. " ++ show ph_level ++ " in " ++ show ph] else Nothing
    let ph_level_error2 = if ph_level > fromJust (level ph_category) then Just ["ph_level " ++ show ph_level ++ " is greater than category level " ++ show (level ph_category) ++ " in " ++ show ph] else Nothing
    _concatenateErrors [ph_level_error, ph_level_error2, validCategory ph_category]
validCategory (Placeholder _ Nothing ph_category) = validCategory ph_category
validCategory MorphismCall{base_morphism=bm, argument=a} = do
    let invalid_arg_error = if not (a `isValidArgumentTo` bm) then Just ["Invalid argument. " ++ show a ++ " is not a valid argument to " ++ show bm] else Nothing
    _concatenateErrors [validCategory bm, validCategory a , invalid_arg_error]
validCategory d@Dereference{base_category=bc, category_id=id} = if isNothing (dereference id bc) then Just ["Dereference not valid in " ++ show d] else Nothing
validCategory l@Label{name=l_name, target=l_target} = do
    let bad_name = if not $ isName l_name then Just ["Labels must have proper Names. Bad name in " ++ show l] else Nothing
    let bad_target = if isNamedCategory l_target then Just ["Labelled a category which already has a name in " ++ show l] else Nothing
    _concatenateErrors [bad_name, bad_target]
validCategory Special{} = Nothing
validCategory Reference{} = Nothing
validCategory im@IntermediateMorphism{chain=chain} =
    let
        invalidTermSequence =  if not (validMorphismTermSequence chain) then Just ["Bad term sequence for MorphismChain in " ++ show im] else Nothing
        definitionNotLabel = Nothing -- todo: put a check here that all definitions have labels
    in
        _concatenateErrors [invalidTermSequence, validCategory (imToMorphism im)]
validCategory r@RefinedCategory{base_category=bc, predicate=p} = do
    let bc_not_placeholder = if not $ isPlaceholder bc then Just ["Refined Categories must be based on a placeholder in " ++ show r] else Nothing
    let predicate_is_morphism = if not $ isMorphism p then Just ["Predicate must be a morphism in refined category " ++ show r] else Nothing
    let predicate_must_accept_bc = if not $ bc `isValidArgumentTo` p then Just ["Predicate must accept placeholder as input argument in " ++ show r] else Nothing
    _concatenateErrors [bc_not_placeholder, predicate_is_morphism, predicate_must_accept_bc]
validCategory other = Just ["validCategory Not implemented for " ++ show other]

isSubstitutable :: Category -> Category -> Bool
isSubstitutable base_category argument =
    case base_category of
        Placeholder{ph_level=ph_level, ph_category=category} -> (level argument) `levelIsContained` ph_level && category `has` argument
        c@Composite{composition_type=Sum, inner=inner_categories} -> or $ map has inner_categories <*> [argument]
        c@Composite{composition_type=Sumposition, inner=inner_categories} -> or $ map has inner_categories <*> [argument]
        _ -> base_category `equal` argument

isValidArgumentTo :: Category -> Category -> Bool
isValidArgumentTo input_argument input_morphic_category
    | not . isMorphic $ input_morphic_category = False
    | otherwise = input (asMorphism input_morphic_category) `isSubstitutable` input_argument

call :: Category -> Category -> Maybe Category
call m@(Morphism input output) input_category
    | isValidArgumentTo input_category m = Just (replace output input input_category)
    | otherwise = Nothing
call m@IntermediateMorphism{chain=(MorphismTerm{m_type=Given,m_category=given_category}:tail)} input_category
    | isValidArgumentTo given_category m =
        case tail of
            [MorphismTerm{m_type=Return, m_category=return_category}] -> Just return_category
            _ -> Just $ IntermediateMorphism tail
    | otherwise = Nothing
call m@IntermediateMorphism{chain=(MorphismTerm{m_type=Definition,m_category=given_category}:tail)} input_category
    | isValidArgumentTo given_category m =
        let
            result = replaceReferences given_category (IntermediateMorphism tail)
        in
            case chain result of
                [MorphismTerm{m_type=Return, m_category=return_category}] -> Just return_category
                _ -> Just result
    | otherwise = Nothing
call m@IntermediateMorphism{chain=other} input_category = Nothing
call c@(Composite Composition _) input_category = call (asMorphism c) input_category
call (Composite Sumposition inner_functions) input_category = tryFunctions input_category (map call inner_functions)
    where
        tryFunctions x = getFirst . mconcat . map (First . ($ x))
call fc@Special{} input_category = Just $ Special{special_type=Flexible}
call base_category fc@Special{}  = Just $ Special{special_type=Flexible}
call r@Reference{} input_category = Just $ Special{special_type=Flexible}
call base_category r@Reference{} = Just $ Special{special_type=Flexible}
call non_morphism _ = Nothing

simplify :: Category -> Category
simplify (Morphism input output) = Morphism (simplify input) (simplify output)
simplify (Composite _ [any]) = simplify any
simplify (Composite composite_type inner) = Composite composite_type (map simplify inner)
simplify ph@(Placeholder placeholder_name ph_level ph_category)
    | isNothing ph_level && ph_category == valid = ph
    | ph_level == level ph_category = simplify ph_category
    | otherwise = Placeholder placeholder_name ph_level (simplify ph_category)
simplify mc@MorphismCall{base_morphism=bm, argument=a} = MorphismCall (simplify bm) (simplify a)
simplify l@Label{target=target} = l{target=simplify target}
simplify input_category = input_category

evaluate :: Category -> Category
evaluate t@Thing{} = t
evaluate Composite{composition_type=ctype, inner=inner} = Composite ctype (map evaluate inner)
evaluate Morphism{input=input, output=output} = Morphism (evaluate input) (evaluate output)
evaluate IntermediateMorphism{chain=chain} = IntermediateMorphism (map (\x -> MorphismTerm{m_type=m_type x, m_category=evaluate (m_category x)}) chain)
evaluate Placeholder{name=name, ph_level=ph_level, ph_category=category} = Placeholder name ph_level (evaluate category)
evaluate MorphismCall{base_morphism=bm, argument=a} = evaluate (fromJust $ call (evaluate bm) (evaluate a))
evaluate Dereference{base_category=bc, category_id=id} = evaluate $ fromJust $ dereference id bc
evaluate s@Special{} = s
evaluate r@Reference{} = r
evaluate l@Label{name=name, target=target}
    | isRecursiveCategory l = replace target Reference{name=name} l
    | otherwise = l
evaluate other = error $ "evaluate Not Implemented yet on " ++ show other

execute :: Category -> Category
execute input_category = do
    let error_checks = validCategory input_category
    case error_checks of
        Nothing -> (simplify . evaluate . simplify) input_category
        Just errors -> error $ "Some semantic errors: \n" ++ unlines errors ++ "\n Found in: " ++ show input_category
