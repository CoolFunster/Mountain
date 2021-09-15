module CategoryCore where

import CategoryData
import CategorySynthesizer

import Debug.Trace
import Data.Monoid hiding (Sum, Product)
import Data.List (find)
import Data.Maybe (mapMaybe, fromJust, isNothing, isJust)

{- Category Functions -}

-- has (containing expr, inner expr)
has :: Category -> Category -> Bool
has big_category small_category 
    | big_category == small_category = True
    | Special{} <- big_category = True
    | Special{} <- small_category = True
    | ForeignCategory{category_type=cat_type} <- big_category = CategoryCore.has cat_type small_category
    | ForeignCategory{category_type=cat_type} <- small_category = CategoryCore.has big_category cat_type 
    | RecursiveCategory{} <- small_category, RecursiveCategory{} <- big_category = CategoryCore.has (unfold Flat big_category) (unfold Flat small_category)
    | RecursiveCategory{} <- small_category = CategoryCore.has big_category (unfold Recursive small_category)
    | RecursiveCategory{} <- big_category = CategoryCore.has (unfold Recursive big_category) small_category
    | isJust (level big_category) && isJust (level small_category) && level big_category < level small_category = False
    | Placeholder{ph_level=ph_level, ph_category=ph_category} <- small_category = (ph_level <= level big_category) && CategoryCore.has big_category ph_category
    | (Composite _ _ [category]) <- big_category = CategoryCore.has category small_category
    | (Composite _ _ [category]) <- small_category = CategoryCore.has big_category category
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
            has_inner (Composite _ big_type []) (Composite _ small_type []) = big_type == small_type
            has_inner (Composite _ big_type []) _ = False
            has_inner (Composite _ Product product_inner) other_category
                | not (isCompositeType Product other_category) = False
                | isCompositeType Product other_category && length product_inner /= length (inner other_category) = False
                | otherwise = and $ zipWith CategoryCore.has product_inner (inner other_category)
            has_inner (Composite _ Sum sum_inner) other_category@(Composite _ Sum _) = all (CategoryCore.has other_category) sum_inner   
            has_inner (Composite _ Sum sum_inner) other_category = or (sequence (map CategoryCore.has sum_inner) other_category)
            {- Higher Categories -}
            has_inner h@Composite{composition_type=Higher} other_category = isAnswered $ synthesizeCategory h other_category
            has_inner big_category h@Composite{composition_type=Higher,inner=inner_terms} = all (has_inner big_category) inner_terms
            {- Composite Morphism -}
            has_inner c_big@Composite{composition_type=Composition} other_category = CategoryCore.has (asMorphism c_big) other_category
            has_inner (Composite _ Sumposition sump_inner) other_category = or (sequence (map CategoryCore.has sump_inner) other_category)
            {- Placeholders -}
            has_inner Placeholder{ph_level=ph_level,ph_category=ph_category} other_category = 
                case (ph_level, level other_category) of
                    (Just level, Just other_level) -> level <= other_level && CategoryCore.has other_category ph_category
                    _ -> error "Bad leveling!"
            has inner RefinedCategory {base_category=_base_category, predicate=_predicate} other = error "Not implemented yet"
            has inner other RefinedCategory {base_category=_base_category, predicate=_predicate} = error "Not implemented yet"

sample :: Category -> Category
sample p@Placeholder{ph_level=Nothing, ph_category=category} = sample p{ph_level=Just 0}
sample p@Placeholder{ph_level=Just desired_level, ph_category=ph_category}
    | level ph_category == Just desired_level = ph_category
    | level ph_category < Just desired_level = error "bad synthesis question"
    | otherwise = sample p{ph_category=reduce ph_category}
sample other = error "unsupported"

reduce :: Category -> Category
reduce t@Thing{} = error "Cannot reduce thing"
reduce c@Composite{composition_type=Product, inner=inner_cats} = c{inner=map reduce inner_cats}
reduce c@Composite{composition_type=Sum, inner=inner_cats} = head inner_cats
reduce c@Composite{composition_type=Higher, inner=inner_cats} = head inner_cats
reduce c@Composite{composition_type=Composition} = asMorphism c
reduce c@Composite{composition_type=Sumposition, inner=inner_cats} = asMorphism c
reduce m@Morphism{input=input_cat, output=output_cat}
    | isNothing $ level m = error "bad morphism"
    | level m == Just 0 = m
    | otherwise = m{input=reduce input_cat, output=reduce output_cat}
reduce p@Placeholder{ph_level=needed_level, ph_category=ph_cat}
    | isNothing needed_level || isNothing (level ph_cat) = error "No idea what usecase this is"
    | level ph_cat == needed_level = reduce ph_cat
    | otherwise = 
        let 
            reduced_cat = reduce ph_cat
        in
            if level reduced_cat == needed_level
                then reduced_cat
                else p{ph_category=reduced_cat}
reduce RefinedCategory{} = error "not supported yet"
reduce Special{special_type=Flexible} = valid
reduce Special{special_type=Universal} = valid
reduce Special{special_type=Reference} = error "can't sample references"
reduce Special{special_type=CategoryData.Any} = valid
reduce ForeignCategory{category_type=ct} = sample ct
reduce r@RecursiveCategory{} = reduce $ unfold Recursive r
reduce m@MorphismCall{base_morphism=bm, argument=a} = sample m{base_morphism=sample bm, argument=sample a}
reduce m@Dereference{base_category=bc, category_id=cid} = fromJust $ dereference cid (sample bc)
reduce m@Membership{} = error "membership not implemented yet"

-- Interpreter functions
replaceReferences :: Category -> Category
replaceReferences t@Thing{} = t
replaceReferences m@Morphism{input=input_cat, output=output_cat} = m{input= replaceReferences input_cat, output=replace (replaceReferences output_cat) Special{name=name input_cat,special_type=Reference} input_cat}
replaceReferences c@Composite{inner=inner} = c{inner=map replaceReferences inner}
replaceReferences p@Placeholder{ph_category=ph_category} = p{ph_category=replaceReferences ph_category}
replaceReferences r@RefinedCategory{base_category=bc, predicate=p} = r{base_category=replaceReferences bc, predicate= replaceReferences p}
replaceReferences s@Special{} = s
replaceReferences fc@ForeignCategory{category_type=ct} = fc{category_type=replaceReferences ct}
replaceReferences rc@RecursiveCategory{inner_expr=recursive_expr} = rc{inner_expr=replaceReferences recursive_expr}
replaceReferences mc@MorphismCall{base_morphism=bm, argument=a} = mc{base_morphism=replaceReferences bm, argument= replaceReferences a}
replaceReferences d@Dereference{base_category=bc} = d{base_category=replaceReferences bc}
replaceReferences m@Membership{big_category=bc, small_category=sc} = m{big_category=replaceReferences bc, small_category=replaceReferences sc}

-- Do not call this function on something that has references
validCategory :: Category -> Bool
validCategory (Thing _) = True
validCategory (Morphism _ m_input m_output) = validCategory m_input && validCategory m_output
validCategory (Composite _ Composition inner) = not (null inner) && all validCategory inner
validCategory (Composite _ Sumposition inner) = not (null inner) && all validCategory inner
validCategory (Composite _ Higher inner) = not (null inner) && all validCategory inner && all isCategoricalObject inner
validCategory (Composite _ _ inner) = all validCategory inner
validCategory ph@(Placeholder _ (Just ph_level) ph_category) = ph_level >= 0 && ph_level <= fromJust (level ph_category) && validCategory ph_category
validCategory (Placeholder _ Nothing ph_category) = empty == ph_category || valid == ph_category
validCategory RecursiveCategory{inner_expr=Morphism{input=fc@Special{special_type=Reference}, output=other_expr}} = fc `elem` freeVariables other_expr
validCategory RecursiveCategory{} = False
validCategory MorphismCall{base_morphism=bm, argument=a} = validCategory bm && validCategory a && a `isValidArgumentTo` bm
validCategory Dereference{base_category=bc, category_id=id} = isJust $ dereference id bc
validCategory ForeignCategory{category_type=ct} = validCategory ct
validCategory Special{} = True
validCategory other = error $ "validCategory Not implemented " ++ show other

isSubstitutable :: Category -> Category -> Bool
{- placeholder substitutions -}
isSubstitutable other p@Placeholder{} = p `isSubstitutable` other
isSubstitutable Placeholder{ph_level=ph_level, ph_category=category} other = category `has` other && level other == ph_level
{- sum type substitutions -}
isSubstitutable c@Composite{composition_type=Sum} other = c `has` other
isSubstitutable c@Composite{composition_type=Sumposition} other = c `has` other
{- all others -}
isSubstitutable a b = a `has` b && b `has` a

isValidArgumentTo :: Category -> Category -> Bool
isValidArgumentTo input_argument input_morphic_category
    | not . isMorphic $ input_morphic_category = False
    | otherwise = input (asMorphism input_morphic_category) `isSubstitutable` input_argument

call :: Category -> Category -> Maybe Category
call m@(Morphism id input output) input_category
    | isValidArgumentTo input_category m = Just (replace output input input_category)
    | otherwise = Nothing
call c@(Composite _ Composition _) input_category = call (asMorphism c) input_category
call (Composite _ Sumposition inner_functions) input_category = tryFunctions input_category (map call inner_functions) 
    where
        tryFunctions x = getFirst . mconcat . map (First . ($ x))
call rc@RecursiveCategory{} input_category
    | not $ isMorphic rc = Nothing
    | otherwise = call (unfold Recursive rc) input_category
call fc@Special{} input_category = Just $ MorphismCall fc input_category
call base_category fc@Special{}  = Just $ MorphismCall base_category fc 
call ForeignCategory{category_type=ct, attached=HaskellFunction attached_call} any_c = attached_call any_c
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
simplify fc@Special{} = fc
simplify input_category = input_category

evaluate :: Category -> Category
evaluate t@Thing{} = t
evaluate Composite{name=name, composition_type=ctype, inner=inner} = Composite name ctype (map evaluate inner)
evaluate Morphism{name=name, input=input, output=output} = Morphism name (evaluate input) (evaluate output)
evaluate Placeholder{name=name, ph_level=ph_level, ph_category=category} = Placeholder name ph_level (evaluate category)
evaluate rc@RecursiveCategory{} = unfold Recursive rc
evaluate MorphismCall{base_morphism=bm, argument=a} = evaluate (fromJust $ call (evaluate bm) (evaluate a))
evaluate Dereference{base_category=bc, category_id=id} = evaluate $ fromJust $ dereference id bc
evaluate fc@ForeignCategory{} = fc
evaluate s@Special{} = s
evaluate other = error $ "evaluate Not Implemented yet on " ++ show other

execute :: Category -> Category
execute input_category
    | not $ validCategory input_category = error $ "Invalid Input Category: " ++ show input_category
    | otherwise = evaluate $ simplify input_category
