module CategorySynthesizer where

import Data.Maybe (catMaybes, fromJust)
import Control.Exception (assert)

import Synthesizers.Synthesizer
import Synthesizers.SequentSynthesizer (sequentSynthesizer)
import CategoryData

isAnswered = Synthesizers.Synthesizer.isAnswered
isNotPossible = Synthesizers.Synthesizer.isNotPossible
isUnknown = Synthesizers.Synthesizer.isUnknown

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
