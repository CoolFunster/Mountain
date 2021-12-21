module CategorySamplerSpec (spec) where

import Test.Hspec
import CategorySynthesizer
import CategoryData
import CategoryCore

import Data.Maybe (fromJust)
import CategorySampler
import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter

nat::Category
nat = Label{
    name=Name "Nat",
    target=Composite{
        composition_type=Sum,
        inner=[
            Thing (Name "0"),
            Morphism valid (Reference (Name "Nat"))
        ]
    }
}

spec :: Spec
spec = do
    describe "sample" $ do
        it "(Thing) requesting a thing of level 0 should return a thing" $ do
            let a = Thing (Name "a")
            let rng = generator (Just 0)
            let (result, _) = randomSample (Placeholder (Name "something") (Specific 0) a) rng
            result`shouldBe` a
        it "(Higher) requesting a thing of a recursive type should return that thing" $ do
            let rng = generator (Just 0)
            let (result, _) = randomSample (Placeholder (Name "x") (Specific 0) nat) rng
            result `shouldBe` fromJust (dereference (Name "0") (target nat))
        it "(natural numbers) should randomly sample natural numbers" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/NaturalNumber.mtpl"
            let simplified_category = CategoryCore.simplify category
            let rng = generator (Just 0)
            print $ categoryToStr simplified_category
            let (result, _) = randomSample Placeholder{name=Unnamed, ph_level=Specific 0, ph_category=simplified_category}  rng
            print $ categoryToStr result