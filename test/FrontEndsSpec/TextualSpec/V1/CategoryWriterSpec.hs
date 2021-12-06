{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryWriterSpec (spec) where

import Test.Hspec

import FrontEnds.Textual.V1.CategoryWriter
import CategoryData


spec :: Spec
spec = do
    describe "CategoryWriter" $ do
        it "(Things) should write things" $ do
            categoryToText Thing{name=Name "Something"} `shouldBe` "`Something" 
        it "(Tuples) should write tuples" $ do
            categoryToText Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]} `shouldBe` "(`a,`b,`c)" 
        it "(Tuples) should write recursive tuples" $ do
            categoryToText Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]} `shouldBe` "(`a,(`b,`c))"
        it "(Sumples) should write sumples" $ do
            categoryToText Composite {name = Unnamed, composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "|`a,`b|"
        it "(Higher) should write higher categories" $ do
            categoryToText Composite {name = Unnamed, composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "^{`a,`b}"
        it "(Composition) should write composite categories" $ do
            categoryToText Composite {name = Unnamed, composition_type = Composition, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "*(`a,`b)"
        it "(Sumposition) should write sumposite categories" $ do
            categoryToText Composite {name = Unnamed, composition_type = Sumposition, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "*|`a,`b|"
        it "(Morphism) should write chains" $ do    
            categoryToText (Morphism Unnamed (Thing (Name "a")) (Morphism Unnamed (Thing (Name "b")) (Thing (Name "c")))) `shouldBe`  "`a -> `b -> `c" 
        it "(Morphism) should write named morphisms" $ do
            categoryToText (Morphism (Name "AB") (Thing (Name "a")) (Thing (Name "b"))) `shouldBe` "AB:`a -> `b" 
        it "(Placeholder) should write placeholders" $ do
            categoryToText Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")} `shouldBe` "x@(`a)"
            categoryToText Placeholder{name=Name "x", ph_level=Just 1, ph_category=Thing (Name "a")} `shouldBe` "x<1>@(`a)" 
        it "(Refinement) should write refinement" $ do
            categoryToText RefinedCategory{name=Unnamed,base_category=Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")}, predicate=Morphism{name=Unnamed, input=Thing (Name "a"), output=Thing (Name "b")}} `shouldBe` "{x@(`a)|`a -> `b}" 
        it "(Special) should write flexible" $ do
            categoryToText Special{name=Unnamed, special_type=Flexible} `shouldBe` "(%)"
            categoryToText Special{name=Name "test", special_type=Flexible} `shouldBe` "test:(%)"
        it "(Special) should write Universal" $ do
            categoryToText Special{name=Unnamed, special_type=Universal} `shouldBe` "%Any"
            categoryToText Special{name=Name "test", special_type=Universal} `shouldBe` "test:%Any"
        it "(Special) should write Reference" $ do
            categoryToText Special{name=Name "Stuff",special_type=Reference} `shouldBe` "$Stuff" 
        it "(Call) should write call" $ do
            categoryToText MorphismCall{base_morphism=Special (Name "base_foo") Reference,argument=Special (Name "some_arg") Reference} `shouldBe` "$base_foo[$some_arg]"
        it "(Call) should write call consecutiveness" $ do
            categoryToText MorphismCall{base_morphism=MorphismCall{base_morphism=Special (Name "base_foo") Reference,argument=Special (Name "some_arg") Reference},argument=Special (Name "some_other") Reference} `shouldBe` "$base_foo[$some_arg][$some_other]"
        it "(Dereference) should write dereferences" $ do
            categoryToText Dereference{base_category=Special{name=Name "base_ref", special_type=Reference}, category_id=Name "name"}  `shouldBe` "$base_ref.name"
        it "(Membership) should write membership" $ do
            categoryToText Membership{big_category=Special (Name "base_category") Reference, small_category=Special (Name "child_category") Reference} `shouldBe` "$base_category::$child_category"