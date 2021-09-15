module CategoriesSpec.NumbersSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)

import Categories.Numbers
import CategoryData
import CategoryCore

spec :: Spec
spec = do
  describe "Natural" $ do
    it "Should have a level of 1" $ do
        level natural `shouldBe` Just 1
    it "Should have zero" $ do
        natural `has` Thing (Name "zero")
    it "Should have one" $ do
        natural `has` Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]}
    it "Should increment properly" $ do
        call increment (Thing (Name "zero")) `shouldBe` Just Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]}
        call increment Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]} `shouldBe` Just Composite{name=Name "succ", composition_type=Product, inner=[Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]}]}
    it "Should decrement properly" $ do
        let expr = call decrement Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]}
        evaluate (fromJust expr) `shouldBe` Thing (Name "zero")
