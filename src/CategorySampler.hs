module CategorySampler where

import Data.Maybe (catMaybes, fromJust, isNothing)
import Control.Exception (assert)

import CategoryData
import CategoryCore

import System.Random
import Debug.Trace
import FrontEnds.Textual.V1.CategoryWriter

generator :: Maybe Int -> StdGen
generator Nothing = mkStdGen 0
generator (Just seed) = mkStdGen seed

selectElemFromList :: (RandomGen g) => [a] -> g -> (a,g)
selectElemFromList input_list generator = (input_list !! rand,new_gen) where
  n = length input_list
  (rand, new_gen) = randomR (0,n-1) generator

randomSample :: Category -> StdGen -> (Category, StdGen)
randomSample p@Placeholder{ph_level=AnyLevel, ph_category=category} rng = randomSample p{ph_level=Specific 0} rng
randomSample p@Placeholder{ph_level=Specific desired_level, ph_category=ph_category} rng
    | level ph_category == Specific desired_level = (ph_category, rng)
    | level ph_category < Specific desired_level = error "bad synthesis question"
    | otherwise = do
        let (reduced_category, new_rng) = tracedReduceOnce ph_category rng
        randomSample p{ph_category=simplify reduced_category} new_rng
randomSample other rng = error "unsupported"

reduceOnce :: Category -> StdGen -> (Category, StdGen)
reduceOnce t@Thing{} rng = error "Cannot reduce thing"
reduceOnce c@Composite{inner=[]} rng = (c, rng)
reduceOnce c@Composite{composition_type=Product, inner=inner_cats} rng
    | level c == Specific 0 = error $ "can't reduce concrete composite: " ++ show c
    | otherwise = do
        let (new_inner_cats, new_rngs) = unzip $ map tracedReduceOnce inner_cats <*> [rng]
        case new_inner_cats of
            [] -> (c{inner=new_inner_cats}, rng)
            _ -> (c{inner=new_inner_cats}, head new_rngs)
reduceOnce c@Composite{composition_type=Sum, inner=inner_cats} rng = selectElemFromList inner_cats rng
reduceOnce c@Composite{composition_type=Higher, inner=inner_cats} rng = error "not implemented yet"
reduceOnce c@Composite{composition_type=Composition} rng
    | level c == Specific 0 = error $ "can't reduce concrete composition: " ++ show c
    | otherwise = tracedReduceOnce (asMorphism c) rng
reduceOnce c@Composite{composition_type=Sumposition, inner=inner_cats} rng = selectElemFromList inner_cats rng
reduceOnce m@Morphism{input=input_cat, output=output_cat} rng
    | level m == Specific 0 = error "Cannot reduce concrete morphism"
    | otherwise = do
        let (reduced_input, new_rng) = tracedReduceOnce input_cat rng
        let resolved_output = simplify (fromJust $ call m reduced_input)
        if level input_cat > level output_cat
            then do
                (m{input=reduced_input, output=resolved_output}, new_rng)
            else do
                let (reduced_output, new_rng2) = tracedReduceOnce resolved_output new_rng
                (m{input=reduced_input, output=reduced_output}, new_rng2)
reduceOnce p@Placeholder{ph_level=needed_level, ph_category=ph_cat} rng
    | level ph_cat == needed_level = error $ "Cannot reduce concrete placeholder: " ++ categoryToStr p
    | otherwise =
        let
            (reduced_cat, new_rng) = tracedReduceOnce ph_cat rng
        in
            (p{ph_category=reduced_cat}, new_rng)
reduceOnce RefinedCategory{} rng = error "not supported yet"
reduceOnce Special{special_type=Flexible} rng = (valid, rng)
reduceOnce Special{special_type=Universal} rng = (valid, rng)
reduceOnce r@Reference{} rng = error $ "can't reduce references. Got " ++ show r
reduceOnce m@MorphismCall{base_morphism=bm, argument=a} rng
    | level m == Specific 0 = error "can't reduce concrete morphism call"
    | otherwise = do
        let (reduced_bm, new_rng1) = if level bm /= Specific 0 then tracedReduceOnce bm rng else (bm, rng)
        let (reduced_a, new_rng2) = if level bm /= Specific 0 then tracedReduceOnce a rng else (a, rng)
        reduceOnce m{base_morphism=reduced_bm, argument=reduced_a} new_rng2
reduceOnce d@Dereference{base_category=base_category, category_id=category_id} rng = error $ "Cannot reduce a dereference. Call simplify first: " ++ categoryToStr d
reduceOnce l@Label{target=target_cat} rng = do
    let unfolded_t = unfold Recursive l
    let (reduced_target, new_rng) = tracedReduceOnce target_cat rng
    if isRecursiveCategory l
        then (reduced_target, new_rng)
        else (l{target=reduced_target}, new_rng)
reduceOnce m@Membership{big_category=bc, small_category=sc} rng = do
    let (reduced_sc, new_rng) = tracedReduceOnce sc rng
    (m{small_category=reduced_sc}, new_rng)
reduceOnce im@IntermediateMorphism{} rng = tracedReduceOnce (imToMorphism im) rng


tracedReduceOnce :: Category -> StdGen -> (Category, StdGen)
-- tracedReduceOnce input_cat = trace (categoryToStr input_cat) (reduceOnce input_cat)
tracedReduceOnce = reduceOnce
