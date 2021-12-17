module CategorySynthesizer where

import Data.Maybe (catMaybes, fromJust, isNothing)
import Control.Exception (assert)

import Synthesizers.Synthesizer
import Synthesizers.SequentSynthesizer (sequentSynthesizer)
import CategoryData

import System.Random

generator :: Maybe Int -> StdGen
generator Nothing = mkStdGen 0
generator (Just seed) = mkStdGen seed

selectElemFromList :: (RandomGen g) => [a] -> g -> (a,g)
selectElemFromList input_list generator = (input_list !! rand,new_gen) where
  n = length input_list
  (rand, new_gen) = randomR (0,n-1) generator

sample :: Category -> StdGen -> (Category, StdGen)
sample p@Placeholder{ph_level=AnyLevel, ph_category=category} rng = sample p{ph_level=Specific 0} rng
sample p@Placeholder{ph_level=Specific desired_level, ph_category=ph_category} rng
    | level ph_category == Specific desired_level = (ph_category, rng)
    | level ph_category < Specific desired_level = error "bad synthesis question"
    | otherwise = do
        let (reduced_category, new_rng) = reduceOnce ph_category rng
        sample p{ph_category=reduced_category} new_rng
sample other rng = error "unsupported"

reduceOnce :: Category -> StdGen -> (Category, StdGen)
reduceOnce t@Thing{} rng = error "Cannot reduce thing"
reduceOnce c@Composite{composition_type=Product, inner=inner_cats} rng
    | level c == Specific 0 = error $ "can't reduce concrete composite: " ++ show c
    | otherwise = do
        let (new_inner_cats, new_rngs) = unzip $ map reduceOnce inner_cats <*> [rng]
        case new_inner_cats of
            [] -> (c{inner=new_inner_cats}, rng)
            _ -> (c{inner=new_inner_cats}, head new_rngs)
reduceOnce c@Composite{composition_type=Sum, inner=inner_cats} rng = selectElemFromList inner_cats rng
reduceOnce c@Composite{composition_type=Higher, inner=inner_cats} rng = error "not implemented yet"
reduceOnce c@Composite{composition_type=Composition} rng
    | level c == Specific 0 = error $ "can't reduce concrete composition: " ++ show c
    | otherwise = reduceOnce (asMorphism c) rng
reduceOnce c@Composite{composition_type=Sumposition, inner=inner_cats} rng = selectElemFromList inner_cats rng
reduceOnce m@Morphism{input=input_cat, output=output_cat} rng
    | AnyLevel == level m = error "bad morphism"
    | level m == Specific 0 = error "Cannot reduce concrete morphism"
    | otherwise = do
        let (reduced_category, new_rng) = reduceOnce input_cat rng
        let resolved_output = replaceReferences input_cat reduced_category
        let (new_output, new_rng2) = reduceOnce resolved_output new_rng
        (m{input=reduced_category, output=new_output}, new_rng2)
reduceOnce p@Placeholder{ph_level=needed_level, ph_category=ph_cat} rng
    | AnyLevel == needed_level || AnyLevel == level ph_cat = error "No idea what usecase this is"
    | level ph_cat == needed_level = error "Cannot reduce ph_category lower than its ph level"
    | otherwise =
        let
            (reduced_cat, new_rng) = reduceOnce ph_cat rng
        in
            (p{ph_category=reduced_cat}, new_rng)
reduceOnce RefinedCategory{} rng = error "not supported yet"
reduceOnce Special{special_type=Flexible} rng = (valid, rng)
reduceOnce Special{special_type=Universal} rng = (valid, rng)
reduceOnce Reference{} rng = error "can't reduce references"
reduceOnce m@MorphismCall{base_morphism=bm, argument=a} rng
    | level m == Specific 0 = error "can't reduce concrete morphism call"
    | otherwise = do
        let (reduced_bm, new_rng1) = if level bm /= Specific 0 then reduceOnce bm rng else (bm, rng)
        let (reduced_a, new_rng2) = if level bm /= Specific 0 then reduceOnce a rng else (a, rng)
        reduceOnce m{base_morphism=reduced_bm, argument=reduced_a} new_rng2
reduceOnce d@Dereference{base_category=base_category, category_id=category_id} rng = do
    let (reduced_base_category, new_rng) = reduceOnce base_category rng
    (d{base_category=reduced_base_category}, new_rng)
reduceOnce l@Label{target=target_cat} rng = do
    let unfolded_t = unfold Recursive l
    let (reduced_target, new_rng) = reduceOnce target_cat rng
    if isRecursiveCategory l 
        then (reduced_target, new_rng)
        else (l{target=reduced_target}, new_rng)
reduceOnce m@Membership{big_category=bc, small_category=sc} rng = do
    let (reduced_sc, new_rng) = reduceOnce sc rng
    (m{small_category=reduced_sc}, new_rng)
reduceOnce im@IntermediateMorphism{} rng = reduceOnce (imToMorphism im) rng

isAnswered = Synthesizers.Synthesizer.isAnswered
isNotPossible = Synthesizers.Synthesizer.isNotPossible
isUnknown = Synthesizers.Synthesizer.isUnknown

synthesizeCategory :: Category -> Category -> SynthesisResult Category
synthesizeCategory = synthesize [sequentSynthesizer]

-- extractSynthesisProblem :: Category -> Category -> Maybe ([Category], Category)
-- extractSynthesisProblem s@Special{name=s_name, special_type=Flexible} input_category 
--     | input_category == s = Just ([valid], Placeholder{name=s_name, ph_level=AnyLevel, ph_category=Special{special_type=Universal}})
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
--                         p@Placeholder{ph_level=AnyLevel} -> Just (fst result, p{ph_level=level})
--                         p@Placeholder{ph_level=Specific ph_level} -> 
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
