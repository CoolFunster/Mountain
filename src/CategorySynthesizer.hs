module CategorySynthesizer where

import Data.Maybe (catMaybes, fromJust, isNothing)
import Control.Exception (assert)

import Synthesizers.Synthesizer
import Synthesizers.SequentSynthesizer (sequentSynthesizer)
import CategoryData

isAnswered = Synthesizers.Synthesizer.isAnswered
isNotPossible = Synthesizers.Synthesizer.isNotPossible
isUnknown = Synthesizers.Synthesizer.isUnknown

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
    | otherwise = (unfold Recursive m){input=reduce input_cat, output=reduce output_cat}
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
reduce Reference{} = error "can't sample references"
reduce m@MorphismCall{base_morphism=bm, argument=a} = sample m{base_morphism=sample bm, argument=sample a}
reduce m@Dereference{base_category=bc, category_id=cid} = fromJust $ dereference cid (sample bc)
reduce l@Label{target=target_cat} = reduce $ unfold Recursive l
reduce m@Membership{} = error "membership not implemented yet"
reduce im@IntermediateMorphism{} = error "IntermediateMorphism not implemented yet"

synthesizeCategory :: Category -> Category -> SynthesisResult Category
synthesizeCategory = synthesize [sequentSynthesizer]

-- extractSynthesisProblem :: Category -> Category -> Maybe ([Category], Category)
-- extractSynthesisProblem s@Special{name=s_name, special_type=Flexible} input_category 
--     | input_category == s = Just ([valid], Placeholder{name=s_name, ph_level=Nothing, ph_category=Special{special_type=Universal}})
--     | not $ s `elem` freeVariables input_category = Nothing
--     | otherwise = 
--         case input_category of
--             Composite{composition_type=c_type, inner=inner} -> 
--                 let 
--                     result_syn_problems = catMaybes $ map (determineSynthesisProblem s) inner
--                 in
--                     assert (length result_syn_problems <= 1) $ case result_syn_problems of
--                         [] -> Nothing
--                         otherwise -> Just $ (Special{special_type=Universal}, head result_syn_problems)
--             Morphism{input=m_input, output=m_output} -> 
--                 if s `elem` freeVariables m_input then determineSynthesisProblem s m_input
--                 else 
--                     let 
--                         result = fromJust $ determineSynthesisProblem s m_output
--                     in
--                         Just (m_input : fst (fromJust result), snd result)
--             Placeholder{ph_level=level, ph_category=ph_category} -> 
--                 let
--                     result = fromJust $ determineSynthesisProblem s ph_category
--                     ph_problem = snd result
--                 in
--                     case ph_problem of
--                         p@Placeholder{ph_level=Nothing} -> Just (fst result, p{ph_level=level})
--                         p@Placeholder{ph_level=Just ph_level} -> 
--                             if ph_level == level then result 
--                             else error "Bad levels in synthesis!!!!"
--                         otherwise -> error "No placeholder as target in synthesis!!!!"
--             RefinedCategory{base_category=bc, predicate=p} ->
--                 error "NOT HANDLED YET"
--             MorphismCall{base_morphism=bm, argument=a} -> 
--                 if s `elem` freeVariables bm then determineSynthesisProblem s bm
--                 else 
--                     let
--                         result = fromJust $ determineSynthesisProblem s a
--                         ph_res = snd result
--                     in 
--                         case ph_res of
--                             Placeholder{ph_category=Special{special_type=Universal}} -> Just (fst result, input bm)
--                             Placeholder{ph_category=other} -> Just (fst result, Composite{
--                                 name=name other ++ "&" ++ name (input bm), 
--                                 composition_type=Product, 
--                                 inner=[other,input bm]})
