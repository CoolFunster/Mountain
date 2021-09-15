module CategoriesSpec.ForeignSpec.HaskellSpec.NumbersSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)

import Categories.Foreign.Haskell.Numbers
import CategoryData
import CategoryCore

import Data.Dynamic

spec :: Spec
spec = do
  describe "haskellInt" $ do
    it "Should have haskell 1" $ do
        1 `shouldBe` 1
        -- haskellInt `has` ForeignCategory{
        --     name=Name "haskellOne",
        --     category_type=Composite{
        --         name=Name "+",
        --         composition_type=Product,
        --         inner=[Composite{name=Name "succ", composition_type=Product, inner=[Thing (Name "zero")]}]
        --     },
        --     attached=HaskellObject (toDyn (1::Int))
        -- }
