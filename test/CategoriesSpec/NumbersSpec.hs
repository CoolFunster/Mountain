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
        natural `has` Thing "zero"
    it "Should have one" $ do
        natural `has` Composite{name="succ", composition_type=Product, inner=[Thing "zero"]}
    it "Should increment properly" $ do
        call increment (Thing "zero") `shouldBe` Just Composite{name="succ", composition_type=Product, inner=[Thing "zero"]}
        call increment Composite{name="succ", composition_type=Product, inner=[Thing "zero"]} `shouldBe` Just Composite{name="succ", composition_type=Product, inner=[Composite{name="succ", composition_type=Product, inner=[Thing "zero"]}]}
