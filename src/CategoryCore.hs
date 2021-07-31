module CategoryCore where

import CategoryData

import Debug.Trace
import Data.Monoid hiding (Sum, Product)
import Data.List (find)
import Data.Maybe (mapMaybe, fromJust, isNothing)

{- Category Functions -}

level :: Category -> Maybe Int
level input_category =
    let
        getInnerLevel inner_categories = 
            case mapMaybe level inner_categories of
                [] -> Nothing
                levels -> Just $ maximum levels
    in
        case input_category of
            (Thing t) -> Just 0
            (Composite _ Higher inner) -> Just (+1) <*> getInnerLevel inner
            (Composite _ _ inner) -> getInnerLevel inner 
            Morphism{input=input,output=output} -> getInnerLevel [input, output]
            Placeholder{ph_level=ph_level} -> ph_level
            RecursiveCategory{inner_expr=inner_expr} -> Just (+1) <*> level inner_expr
            MorphismCall{base_morphism=bm} -> level $ output bm
            FlexibleCategory{} -> Nothing
            Dereference{base_category=bc,category_id=id} -> 

-- has (containing expr, inner expr)
has :: Category -> Category -> Bool
has big_category small_category 
    | big_category == small_category = True
    | FlexibleCategory{} <- big_category = True
    | FlexibleCategory{} <- small_category = True
    | RecursiveCategory{} <- small_category, RecursiveCategory{} <- big_category = has (unfold Flat big_category) (unfold Flat small_category)
    | RecursiveCategory{} <- small_category = has big_category (unfold Recursive small_category)
    | RecursiveCategory{} <- big_category = has (unfold Recursive big_category) small_category
    | level big_category < level small_category = False
    | Placeholder{ph_level=ph_level, ph_category=ph_category} <- small_category = (ph_level <= level big_category) && has big_category ph_category
    | (Composite _ _ [category]) <- big_category = has category small_category
    | (Composite _ _ [category]) <- small_category = has big_category category
    | otherwise = has_inner big_category small_category
        where 
            {- Things -}
            -- equality is checked above only here if 2 things not eq
            has_inner (Thing t) _ = False
            {- Morphisms -}
            has_inner Morphism{input=input1,output=output1} Morphism{input=input2,output=output2} = has input1 input2 && skip_intermediate_has output1 output2
                where
                    skip_intermediate_has a b
                        | not (isMorphism b) = has a b 
                        | isMorphism b = has a b || skip_intermediate_has a (output b)
            has_inner m_big@Morphism{} c_small@Composite{} = has m_big (asMorphism c_small)
            has_inner Morphism{} _ = False
            {- Composite Categories -}
            has_inner (Composite _ big_type []) (Composite _ small_type []) = big_type == small_type
            has_inner (Composite _ big_type []) _ = False
            {- TODO: Once synthesis is in, add to higher composite categories -}
            has_inner (Composite _ Higher higher_inner) other_category = has (Composite "" Sum higher_inner) other_category
            has_inner (Composite _ Product product_inner) other_category
                | not (isCompositeType Product other_category) = False
                | isCompositeType Product other_category && length product_inner /= length (inner other_category) = False
                | otherwise = and $ zipWith has product_inner (inner other_category)
            has_inner (Composite _ Sum sum_inner) other_category@(Composite _ Sum _) = all (has other_category) sum_inner   
            has_inner (Composite _ Sum sum_inner) other_category = or (sequence (map has sum_inner) other_category)
            {- Composite Morphism -}
            has_inner c_big@Composite{composition_type=Composition} other_category = has (asMorphism c_big) other_category
            has_inner (Composite _ Sumposition sump_inner) other_category = or (sequence (map has sump_inner) other_category)
            {- Placeholders -}
            has_inner Placeholder{ph_level=ph_level,ph_category=ph_category} other_category = 
                case (ph_level, level other_category) of
                    (Just level, Just other_level) -> level <= other_level && has other_category ph_category
                    _ -> error "Bad leveling!"

freeVariables :: Category -> [Category]
freeVariables (Thing _) = []
freeVariables (Morphism _ input output) = freeVariables input ++ freeVariables output
freeVariables (Composite _ _ inner) = concatMap freeVariables inner
freeVariables ph@(Placeholder _ _ ph_category) = ph : freeVariables ph_category
freeVariables rc@RecursiveCategory{} = freeVariables $ unfold Flat rc
freeVariables MorphismCall{base_morphism=bm, argument=a} = freeVariables bm ++ freeVariables a
freeVariables f@FlexibleCategory{} = [f]

-- replace :: base_expr -> old -> new -> new_expr 
-- idea compare base expr to old. if same replace with new. otherwise return old. recurse
replace :: Category -> Category -> Category -> Category
replace base_expr old new 
    | base_expr == old = new
    | otherwise = 
        case base_expr of
            Thing _ -> base_expr
            Composite id composite_type inner -> Composite id composite_type (sequence (sequence (map replace inner) old) new)
            Morphism id input output -> Morphism id (replace input old new) (replace output old new)
            Placeholder id ph_level ph_category -> Placeholder id ph_level (replace ph_category old new)
            MorphismCall bm a -> MorphismCall (replace bm old new) (replace a old new)
            rc@RecursiveCategory{} -> RecursiveCategory $ replace (inner_expr rc) old new
            FlexibleCategory{} -> base_expr

data UnfoldType = Flat | Recursive
unfold :: UnfoldType -> Category -> Category
unfold Flat RecursiveCategory{inner_expr=Morphism{output=rest_of_expr}} = rest_of_expr
unfold Recursive rc@RecursiveCategory{inner_expr=Morphism{input=rec_ph, output=rest_of_expr}} = replace rest_of_expr rec_ph rc
unfold _ _ = error "Bad input"

makeRecursiveCategory :: Id -> Category -> Category
makeRecursiveCategory rec_ph_name rec_cat 
    | FlexibleCategory{name=rec_ph_name} `notElem` freeVariables rec_cat = error $ "Can't make recursive expr. Does not have a recursive ph of name " ++ show rec_ph_name
    | otherwise = RecursiveCategory{inner_expr=Morphism{name=rec_ph_name,input=FlexibleCategory{name=rec_ph_name},output=rec_cat}}


isMorphic :: Category -> Bool
isMorphic Morphism{} = True
isMorphic Composite{composition_type=Sumposition} = True
isMorphic Composite{composition_type=Composition} = True
isMorphic MorphismCall{base_morphism=Morphism{output=Morphism{}}} = True
isMorphic RecursiveCategory{inner_expr=Morphism{output=rec_category}} = isMorphic rec_category
isMorphic FlexibleCategory{} = True
isMorphic _ = False

asMorphism :: Category -> Category
asMorphism input_category
    | not . isMorphic $ input_category = error $ "unable to represent as morphism: " ++ show input_category
    | otherwise =
        case input_category of
            m@Morphism{} -> m
            (Composite _ Composition comp_inner) -> Morphism "" (input (head morphised_comp_inner)) (output (last morphised_comp_inner))
                where  
                    morphised_comp_inner = map asMorphism comp_inner
            (Composite _ Sumposition comp_inner) -> Morphism "" (Composite "" Sum (map (input . asMorphism) comp_inner)) (Composite "" Sum (map (output . asMorphism) comp_inner))
            MorphismCall{base_morphism=bm} -> asMorphism $ output bm
            f@FlexibleCategory{name=f_name} -> Morphism f_name f f
            rc@RecursiveCategory{} -> asMorphism $ unfold Recursive rc
            something -> error $ "not covered: " ++ show something


dereference :: Category -> Id -> Maybe Category
dereference cat@(Morphism _ input output) reference =
    case reference of
        "input" -> Just input
        "output" -> Just output
        _ -> Nothing
dereference cat@(Composite _ _ inner) reference = 
    case find (\x -> name x == reference) inner of
        Just x -> Just x
        _ -> Nothing
dereference FlexibleCategory{} id = Just FlexibleCategory{name=id}
dereference _ reference = Nothing

mtplCurry :: Id -> [Category] -> Category
mtplCurry m_name [] = empty
mtplCurry m_name [any] = any
mtplCurry m_name (head:tail) = Morphism m_name head (mtplCurry m_name tail)

composable :: Category -> Category -> Bool
composable former_category latter_category
    | not (isMorphic former_category) || not (isMorphic latter_category) = False
    | otherwise = has (output (asMorphism former_category)) (input (asMorphism latter_category))

categoricallyEqual :: Category -> Category -> Bool
categoricallyEqual a b = a `has` b && b `has` a


-- Interpreter functions
validCategory :: Category -> Bool
validCategory (Thing _) = True
validCategory (Morphism _ m_input m_output) = validCategory m_input && validCategory m_output
validCategory (Composite _ Composition inner) = not (null inner) && all validCategory inner
validCategory (Composite _ Sumposition inner) = not (null inner) && all validCategory inner
validCategory (Composite _ Higher inner) = not (null inner) && all validCategory inner
validCategory (Composite _ _ inner) = all validCategory inner
validCategory ph@(Placeholder _ (Just ph_level) ph_category) = ph_level >= 0 && ph_level <= fromJust (level ph_category) && validCategory ph_category
validCategory (Placeholder _ Nothing ph_category) = empty == ph_category || valid == ph_category
validCategory RecursiveCategory{inner_expr=Morphism{input=fc@FlexibleCategory{}, output=other_expr}} = fc `elem` freeVariables other_expr
validCategory RecursiveCategory{} = False
validCategory MorphismCall{base_morphism=bm, argument=a} = validCategory bm && validCategory a && a `isValidArgumentTo` bm
validCategory FlexibleCategory{} = True

isValidArgumentTo :: Category -> Category -> Bool
isValidArgumentTo input_argument input_morphic_category
    | not . isMorphic $ input_morphic_category = False
    | otherwise = input (asMorphism input_morphic_category)  `has` input_argument

call :: Category -> Category -> Maybe Category
call (Morphism id input output) input_category
    | input == input_category = Just output
    | isPlaceholder input && not (isPlaceholder input_category) && has (ph_category input) input_category = Just (replace (output) input input_category)
    | isPlaceholder input && isPlaceholder input_category && level input >= level input_category && has (ph_category input) input_category = Just (Morphism id input_category (replace (output) input input_category))
call c@(Composite _ Composition _) input_category = call (asMorphism c) input_category
call (Composite _ Sumposition inner_functions) input_category = tryFunctions input_category (map call inner_functions) 
    where
        tryFunctions x = getFirst . mconcat . map (First . ($ x))
call rc@RecursiveCategory{} input_category
    | not $ isMorphic rc = Nothing
    | otherwise = call (unfold Recursive rc) input_category
call fc@FlexibleCategory{} input_category = Just $ MorphismCall fc input_category
call base_category fc@FlexibleCategory{}  = Just $ MorphismCall base_category fc 
call non_morphism _ = Nothing

simplify :: Category -> Category
simplify (Morphism name input output) = Morphism name (simplify input) (simplify output)
simplify (Composite composite_name _ []) = valid
simplify (Composite composite_name _ [any]) = simplify any
simplify (Composite composite_name composite_type inner) = Composite composite_name composite_type (map simplify inner)
simplify ph@(Placeholder placeholder_name ph_level ph_category)
    | isNothing ph_level && ph_category == valid = ph
    | ph_level == level ph_category = simplify ph_category
    | otherwise = Placeholder placeholder_name ph_level (simplify ph_category)
simplify RecursiveCategory{inner_expr=inner} = RecursiveCategory $ simplify inner
simplify mc@MorphismCall{base_morphism=bm, argument=a}
    | not . isRecursiveCategory $ bm = fromJust $ call (simplify bm) (simplify a)
    | otherwise = MorphismCall (simplify bm) (simplify a)

simplify fc@FlexibleCategory{} = fc
simplify input_category = input_category

evaluate :: Category -> Category
evaluate t@Thing{} = t
evaluate Composite{name=name, composition_type=ctype, inner=inner} = Composite name ctype (map evaluate inner)
evaluate Morphism{name=name, input=input, output=output} = Morphism name (evaluate input) (evaluate output)
evaluate Placeholder{name=name, ph_level=ph_level, ph_category=category} = Placeholder name ph_level (evaluate category)
evaluate rc@RecursiveCategory{} = unfold Recursive rc
evaluate MorphismCall{base_morphism=bm, argument=a} = evaluate (fromJust $ call (evaluate bm) (evaluate a))
evaluate fc@FlexibleCategory{} = fc

execute :: Category -> Category
execute input_category
    | not $ validCategory input_category = error $ "Invalid Input Category: " ++ show input_category
    | otherwise = evaluate $ simplify input_category
