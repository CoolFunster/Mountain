module CategorySynthesizerSpec (spec) where

import Test.Hspec
import CategorySynthesizer
import CategoryData
import Data.Maybe (fromJust)

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
            sample (Placeholder (Name "something") (Specific 0) a) `shouldBe` a
        it "(Higher) requesting a thing of a recursive type should return that thing" $ do
            sample (Placeholder (Name "x") (Specific 0) nat) `shouldBe` fromJust (dereference (Name "0") (target nat))