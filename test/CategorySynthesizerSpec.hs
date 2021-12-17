module CategorySynthesizerSpec (spec) where

import Test.Hspec
import CategorySynthesizer
import CategoryData

import Data.Maybe (fromJust)
import CategorySynthesizer (generator)
import Data.Ix (Ix(range))

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
            let (result, _) = sample (Placeholder (Name "something") (Specific 0) a) rng
            result`shouldBe` a
        it "(Higher) requesting a thing of a recursive type should return that thing" $ do
            let rng = generator (Just 0)
            let (result, _) = sample (Placeholder (Name "x") (Specific 0) nat) rng
            result `shouldBe` fromJust (dereference (Name "0") (target nat))