module CategoryCore where

import CategoryData
import Debug.Trace

{- Some basic category types -}
valid :: Category
valid = Composite "valid" Product []

empty :: Category
empty = Composite "empty" Sum []

{- Category Properties and Functions -}

level :: Category -> Int
level (Thing t) = 0
level (Composite _ Higher inner) = 1 + maximum (map level inner)
level (Composite _ _ inner) = maximum (map level inner)
level Morphism {input=input,output=output} = maximum (map level [input, output])
level Placeholder{ph_level=ph_level} = ph_level

-- has (containing expr, inner expr)
has :: Category -> Category -> Bool
has big_category small_category 
    | big_category == small_category = True
    | level big_category < level small_category = False
    | otherwise = has_inner big_category small_category
        where 
            {- length 1 algebraic identity rules -}
            has_inner (Composite _ _ [category]) other_category = has category other_category
            has_inner category (Composite _ _ [other_category]) = has category other_category
            {- Things -}
            has_inner (Thing t) _ = False
            {- Morphisms -}
            has_inner m1@Morphism{input=input1,output=output1} m2@Morphism{input=input2,output=output2} = m1 == m2 || (has input1 input2 && skip_intermediate_has output1 output2)
                where
                    skip_intermediate_has a b
                        | not (isMorphism b) = has a b 
                        | isMorphism b = has a b || skip_intermediate_has a (output b)
            has_inner m_big@Morphism{} c_small@Composite{} = has m_big (compositionAsMorphism c_small)
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
            has_inner c_big@Composite{composition_type=Composition} other_category = has (compositionAsMorphism c_big) other_category
            has_inner (Composite _ Sumposition sump_inner) other_category = or (sequence (map has sump_inner) other_category)
            {- Placeholders -}
            has_inner Placeholder{ph_level=ph_level,ph_category=ph_category} other_category = ph_level <= level other_category && has other_category ph_category

freeVariables :: Category -> [Category]
freeVariables (Thing _) = []
freeVariables (Morphism _ input output) = freeVariables input ++ freeVariables output
freeVariables (Composite _ _ inner) = concatMap freeVariables inner
freeVariables ph@(Placeholder _ _ ph_category) = ph : freeVariables ph_category

-- replace :: base_expr -> old -> new -> new_expr 
-- idea compare base expr to old. if same replace with new. otherwise return old
replace :: Category -> Category -> Category -> Category
replace base_expr old new 
    | base_expr == old = new
    | Composite id composite_type inner <- base_expr = Composite id composite_type (sequence (sequence (map replace inner) old) new)
    | Morphism id input output <- base_expr = Morphism id (replace input old new) (replace output old new)
    | Placeholder id ph_level ph_category <- base_expr = Placeholder id ph_level (replace ph_category old new)

call :: Category -> Category -> Category
call (Morphism id input output) input_category
    | input == input_category = output
    | isPlaceholder input && not (isPlaceholder input_category) && has (ph_category input) input_category = replace (output) input input_category
    | isPlaceholder input && isPlaceholder input_category && level input >= level input_category && has (ph_category input) input_category = Morphism id input_category (replace (output) input input_category)


validCategory :: Category -> Bool
validCategory (Thing _) = True
validCategory (Morphism _ input output) = validCategory input && validCategory output
validCategory (Composite _ _ inner) = all validCategory inner
validCategory ph@(Placeholder _ ph_level ph_category) = ph_level >= 0 && ph_level <= level ph_category && validCategory ph_category
