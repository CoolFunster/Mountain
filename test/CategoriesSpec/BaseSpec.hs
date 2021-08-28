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
            name="Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name="Base",special_type=Reference},
                    category_id="has"
                },
                argument=Composite{
                    name="has_arg",
                    composition_type=Product,
                    inner=[
                        boolean,
                        Thing "true"
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
            name="Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name="Base",special_type=Reference},
                    category_id="has"
                },
                argument=Composite{
                    name="has_arg",
                    composition_type=Product,
                    inner=[
                        boolean,
                        Thing "true"
                    ]
                }
            }
        }
        (validCategory . output) test_category `shouldBe` True
        let result = call test_category base
        let eval_result = Just evaluate <*> result
        eval_result `shouldBe` Just (fromJust (dereference "true" boolean))
    it "Should properly call isValidArgumentTo" $ do
        let raw_category = Morphism{
            name="Test_category",
            input=base,
            output=MorphismCall{
                base_morphism=Dereference{
                    base_category=Special{name="Base",special_type=Reference},
                    category_id="isValidArgumentTo"
                },
                argument=Composite{
                    name="has_arg",
                    composition_type=Product,
                    inner=[
                        Thing "true",
                        Morphism{
                            name="some_random_morphism",
                            input=Placeholder{name="x", ph_level=Just 0, ph_category=boolean},
                            output=Special{name="x",special_type=Reference}
                        }
                    ]
                }
            }
        }
        print raw_category
        let test_category = replaceReferences raw_category
        Placeholder{name="x", ph_level=Just 0, ph_category=boolean} `isSubstitutable` Thing "true" `shouldBe` True

        let result = call test_category base
        let eval_result = Just evaluate <*> result
        eval_result `shouldBe` Just (fromJust (dereference "true" boolean))
