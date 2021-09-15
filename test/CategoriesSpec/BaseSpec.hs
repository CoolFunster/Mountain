module CategoriesSpec.BaseSpec (spec) where

import Test.Hspec
import Data.Maybe (fromJust)

import Categories.Base
import CategoryData
import CategoryCore

spec :: Spec
spec = do
  describe "Foreign Has" $ do
    it "Should properly call has" $ do
        let test_category = replaceReferences Morphism{
            name=Name "Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name=Name "Base",special_type=Reference},
                    category_id=Name "has"
                },
                argument=Composite{
                    name=Name "has_arg",
                    composition_type=Product,
                    inner=[
                        boolean,
                        Thing (Name "true")
                    ]
                }
            }
        }
        (validCategory . output) test_category `shouldBe` True
        let result = call test_category base
        let eval_result = Just evaluate <*> result
        print eval_result
    it "Should properly call has" $ do
        let test_category = replaceReferences Morphism{
            name=Name "Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name=Name "Base",special_type=Reference},
                    category_id=Name "has"
                },
                argument=Composite{
                    name=Name "has_arg",
                    composition_type=Product,
                    inner=[
                        boolean,
                        Thing (Name "true")
                    ]
                }
            }
        }
        (validCategory . output) test_category `shouldBe` True
        let result = call test_category base
        let eval_result = Just evaluate <*> result
        eval_result `shouldBe` Just (fromJust (dereference (Name "true") boolean))
    it "Should properly call isValidArgumentTo" $ do
        let raw_category = Morphism{
            name=Name "Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name=Name "Base",special_type=Reference},
                    category_id=Name "isValidArgumentTo"
                },
                argument=Composite{
                    name=Name "has_arg",
                    composition_type=Product,
                    inner=[
                        Thing (Name "true"),
                        Morphism{
                            name=Name "some_random_morphism",
                            input=Placeholder{name=Name "x", ph_level=Just 0, ph_category=boolean},
                            output=Special{name=Name "x",special_type=Reference}
                        }
                    ]
                }
            }
        }
        -- print raw_category
        let test_category = replaceReferences raw_category
        Placeholder{name=Name "x", ph_level=Just 0, ph_category=boolean} `isSubstitutable` Thing (Name "true") `shouldBe` True

        let result = call test_category base
        let eval_result = Just evaluate <*> result
        eval_result `shouldBe` Just (fromJust (dereference (Name "true") boolean))
