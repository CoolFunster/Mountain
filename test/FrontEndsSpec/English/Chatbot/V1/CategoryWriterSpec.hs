{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.English.Chatbot.V1.CategoryWriterSpec (spec) where

import Test.Hspec

import FrontEnds.English.Chatbot.V1.CategoryWriter
import CategoryData


spec :: Spec
spec = do
    describe "CategoryWriter" $ do
        it "(Things) should write things" $ do
            categoryToText Thing{name=Name "Something"} `shouldBe` "A THING Something" 
        it "(Tuples) should write tuples" $ do
            categoryToText Composite {composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]} `shouldBe` "A THING a, A THING b, AND A THING c"
        it "(Tuples) should write recursive tuples" $ do
            categoryToText Composite {composition_type = Product, inner = [Thing {name = Name "a"},Composite {composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]} `shouldBe` "A THING a, AND A THING b, AND A THING c"
        it "(Sumples) should write sumples" $ do
            categoryToText Composite {composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "A THING a, OR A THING b"
        it "(Higher) should write higher categories" $ do
            categoryToText Composite {composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "A STRUCTURE WHICH HAS A THING a, AND A THING b"
        it "(Composition) should write composite categories" $ do
            categoryToText Composite {composition_type = Composition, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "A CHAIN OF FUNCTIONS WHERE A THING a IS GIVEN TO A THING b"
        it "(Sumposition) should write sumposite categories" $ do
            categoryToText Composite {composition_type = Sumposition, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "IF A THING a, A THING a, A THING b"
        it "(Morphism) should write chains" $ do    
            categoryToText (Morphism (Thing (Name "a")) (Morphism (Thing (Name "b")) (Thing (Name "c")))) `shouldBe`  "GIVEN A THING a, GIVEN A THING b, RETURN A THING c"
        it "(Morphism) should write named morphisms" $ do
            categoryToText (Morphism (Thing (Name "a")) (Thing (Name "b"))) `shouldBe` "GIVEN A THING a, RETURN A THING b" 
        it "(Morphism) should handle nested chains" $ do
            categoryToText (Morphism (Morphism (Thing (Name "a")) (Thing (Name "b"))) (Thing (Name "b"))) `shouldBe` "GIVEN A FUNCTION (GIVEN A THING a, RETURN A THING b), RETURN A THING b"
        it "(Placeholder) should write placeholders" $ do
            categoryToText Placeholder{name=Name "x", ph_level=AnyLevel, ph_category=Thing (Name "a")} `shouldBe` "SOME CONCEPT CALLED x WHICH IS A THING a"
            categoryToText Placeholder{name=Name "x", ph_level=Specific 1, ph_category=Thing (Name "a")} `shouldBe` "SOME TYPE CALLED x WHICH IS A THING a"
        it "(Refinement) should write refinement" $ do
            categoryToText RefinedCategory{base_category=Placeholder{name=Name "x", ph_level=AnyLevel, ph_category=Thing (Name "a")}, predicate=Morphism{input=Thing (Name "a"), output=Thing (Name "b")}} `shouldBe` "SOME CONCEPT CALLED x WHICH IS A THING a WHICH GIVEN A THING a, RETURN A THING b"
        it "(Special) should write flexible" $ do
            categoryToText Special{special_type=Flexible} `shouldBe` "SOMETHING"
        it "(Special) should write Universal" $ do
            categoryToText Special{special_type=Universal} `shouldBe` "ANYTHING"
        it "(Special) should write Reference" $ do
            categoryToText Reference{name=Name "Stuff"} `shouldBe` "Stuff" 
        it "(Call) should write call" $ do
            categoryToText MorphismCall{base_morphism=Reference (Name "base_foo"),argument=Reference (Name "some_arg") } `shouldBe` "GIVE some_arg TO base_foo"
        it "(Call) should write call consecutiveness" $ do
            categoryToText MorphismCall{base_morphism=MorphismCall{base_morphism=Reference (Name "base_foo") ,argument=Reference (Name "some_arg") },argument=Reference (Name "some_other") } `shouldBe` "GIVE some_other TO GIVE some_arg TO base_foo"
        it "(Dereference) should write dereferences" $ do
            categoryToText Dereference{base_category=Reference{name=Name "base_ref"}, category_id=Name "name"}  `shouldBe` "THE name OF base_ref"
        it "(Membership) should write membership" $ do
            categoryToText Membership{big_category=Reference (Name "base_category"), small_category=Reference (Name "child_category") } `shouldBe` "A base_category WHICH IS child_category"
        it "(IntermediateMorphism) should write IM" $ do
            categoryToText IntermediateMorphism{chain=[MorphismTerm{m_type=Given, m_category=Thing (Name "a")}, MorphismTerm{m_type=Return, m_category=Thing (Name "b")}]} `shouldBe` "GIVEN A THING a, RETURN A THING b"