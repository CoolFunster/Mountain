module CategorySampler where

import Data.Maybe (catMaybes, fromJust, isNothing)
import Control.Exception (assert)

import CategoryData
import CategoryCore

import System.Random
import Debug.Trace
import FrontEnds.Textual.V1.CategoryWriter
import GHC.Arr (badSafeIndex)

generator :: Maybe Int -> StdGen
generator Nothing = mkStdGen 0
generator (Just seed) = mkStdGen seed

selectElemFromList :: (RandomGen g) => [a] -> g -> (a,g)
selectElemFromList input_list generator = (input_list !! rand,new_gen) where
  n = length input_list
  (rand, new_gen) = randomR (0,n-1) generator

nextSeed :: (RandomGen g) => g -> g
nextSeed generator = do
    let (_, new_gen) = randomR (0,1 :: Int) generator
    new_gen

removeBloat :: Category -> Category
removeBloat t@Thing{} = t
removeBloat c@Composite{inner=inner} = c{inner=map removeBloat inner}
removeBloat m@Morphism{input=i, output=o} = m{input=removeBloat i, output=removeBloat o}
removeBloat p@Placeholder{ph_category=pc} = p{ph_category=removeBloat pc}
removeBloat r@RefinedCategory{base_category=bc, predicate=p} = r{base_category=removeBloat bc, predicate=removeBloat p}
removeBloat s@Special{} = s
removeBloat l@Label{target=t} = if isRecursiveCategory l then l{target=removeBloat t} else removeBloat t
removeBloat r@Reference{} = r
removeBloat m@MorphismCall{base_morphism=bm, argument=c} = m{base_morphism=removeBloat bm, argument=removeBloat c}
removeBloat d@Dereference{base_category=bc} = d{base_category=simplify bc}
removeBloat Membership{small_category=sc} = sc
removeBloat im@IntermediateMorphism{} = removeBloat (asMorphism im)

randomSample :: Category -> StdGen -> (Category, StdGen)
randomSample p@Placeholder{ph_level=AnyLevel, ph_category=category} rng = randomSample p{ph_level=Specific 0} (nextSeed rng)
randomSample p@Placeholder{ph_level=Specific desired_level, ph_category=ph_category} rng
    | level ph_category == Specific desired_level = (ph_category, nextSeed rng)
    | level ph_category < Specific desired_level = error "bad synthesis question"
    | otherwise = do
        let (reduced_category, new_rng) = tracedReduceOnce (removeBloat ph_category) rng
        let (sampled_category, new_rng2) = randomSample p{ph_category=simplify reduced_category} new_rng
        (removeBloat sampled_category, nextSeed new_rng2)
randomSample other rng = error "unsupported"

reduceOnce :: Category -> StdGen -> (Category, StdGen)
reduceOnce t@Thing{} rng = error "Cannot reduce thing"
reduceOnce c@Composite{inner=[]} rng = (c, nextSeed rng)
reduceOnce c@Composite{composition_type=Product, inner=inner_cats} rng
    | level c == Specific 0 = error $ "can't reduce concrete composite: " ++ show c
    | otherwise = do
        let (new_inner_cats, new_rngs) = unzip $ map tracedReduceOnce inner_cats <*> [rng]
        case new_inner_cats of
            [] -> (c{inner=new_inner_cats}, rng)
            _ -> (c{inner=new_inner_cats}, nextSeed rng)
reduceOnce c@Composite{composition_type=Sum, inner=inner_cats} rng = do
    let (cat_result, next_rng) = selectElemFromList inner_cats rng
    (cat_result, nextSeed next_rng)
reduceOnce c@Composite{composition_type=Higher, inner=inner_cats} rng = error "not implemented yet"
reduceOnce c@Composite{composition_type=Composition} rng
    | level c == Specific 0 = error $ "can't reduce concrete composition: " ++ show c
    | otherwise = tracedReduceOnce (asMorphism c) (nextSeed rng)
reduceOnce c@Composite{composition_type=Sumposition, inner=inner_cats} rng = do
    let (cat_result, next_rng) = selectElemFromList inner_cats rng
    (cat_result, nextSeed next_rng)
reduceOnce m@Morphism{input=input_cat@Placeholder{}, output=output_cat} rng
    | level m == Specific 0 = error "Cannot reduce concrete morphism"
    | otherwise = do
        let (reduced_input, new_rng) = tracedReduceOnce input_cat rng
        let new_output = case reduced_input of
                p@Placeholder{} -> output_cat
                anything_else -> replaceReferences output_cat Label{name=name input_cat, target=anything_else}
        if level reduced_input < level new_output
            then do
                let (reduced_output, new_rng2) = tracedReduceOnce new_output new_rng
                (m{input=reduced_input, output=reduced_output}, nextSeed new_rng2)
            else do
                (m{input=reduced_input, output=new_output}, nextSeed new_rng)
reduceOnce m@Morphism{input=input_cat, output=output_cat} rng
    | level m == Specific 0 = error "Cannot reduce concrete morphism"
    | otherwise = do
        let (reduced_output, new_rng) = tracedReduceOnce output_cat rng
        (m{input=input_cat, output=reduced_output}, nextSeed new_rng)
reduceOnce p@Placeholder{ph_level=needed_level, ph_category=ph_cat} rng
    | level ph_cat == needed_level = error $ "Cannot reduce concrete placeholder: " ++ categoryToStr p
    | otherwise =
        let
            (reduced_cat, new_rng) = tracedReduceOnce ph_cat rng
        in
            if level reduced_cat == Specific 0
                then (reduced_cat, nextSeed new_rng)
                else (p{ph_category=reduced_cat}, nextSeed new_rng)
reduceOnce RefinedCategory{} rng = error "not supported yet"
reduceOnce Special{special_type=Flexible} rng = (valid, nextSeed rng)
reduceOnce Special{special_type=Universal} rng = (valid, nextSeed rng)
reduceOnce r@Reference{} rng = error $ "can't reduce references. Got " ++ show r
reduceOnce m@MorphismCall{base_morphism=bm, argument=a} rng
    | level m == Specific 0 = error "can't reduce concrete morphism call"
    | otherwise = do
        let (reduced_bm, new_rng1) = if level bm /= Specific 0 then tracedReduceOnce bm rng else (bm, rng)
        let (reduced_a, new_rng2) = if level bm /= Specific 0 then tracedReduceOnce a rng else (a, rng)
        tracedReduceOnce m{base_morphism=reduced_bm, argument=reduced_a} new_rng2
reduceOnce d@Dereference{base_category=base_category, category_id=category_id} rng = error $ "Cannot reduce a dereference. Call simplify first: " ++ categoryToStr d
reduceOnce l@Label{target=target_cat} rng = do
    if isRecursiveCategory l
        then do
            let unfolded_l = unfold Recursive l
            tracedReduceOnce unfolded_l rng
        else tracedReduceOnce target_cat rng
reduceOnce m@Membership{big_category=bc, small_category=sc} rng = do
    let (reduced_sc, new_rng) = tracedReduceOnce sc rng
    (reduced_sc, nextSeed new_rng)
reduceOnce im@IntermediateMorphism{} rng = tracedReduceOnce (imToMorphism im) rng


tracedReduceOnce :: Category -> StdGen -> (Category, StdGen)
tracedReduceOnce input_cat rng = do
    let reduced_input = reduceOnce input_cat rng
    -- trace (categoryToStr input_cat ++ " ===>>> " ++ categoryToStr (fst reduced_input)) reduced_input
    reduced_input
