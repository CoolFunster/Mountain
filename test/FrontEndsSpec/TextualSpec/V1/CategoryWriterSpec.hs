{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module FrontEndsSpec.TextualSpec.V1.CategoryWriterSpec (spec) where

import Test.Hspec

import FrontEnds.Textual.V1.CategoryWriter
import Category


spec :: Spec
spec = do
    describe "CategoryWriter" $ do
        it "(Things) should write things" $ do
            categoryToString (Thing (Name "Something")) `shouldBe` "`Something"
        it "(Tuples) should write tuples" $ do
            categoryToString Composite{composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]} `shouldBe` "(`a,`b,`c)"
        it "(Tuples) should write recursive tuples" $ do
            categoryToString Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]} `shouldBe` "(`a,(`b,`c))"
        it "(Eithers) should write sumples" $ do
            categoryToString Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "|`a,`b|"
        it "(Composition) should write composite categories" $ do
            categoryToString Composite {composite_type = Composition, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "*(`a,`b)*"
        it "(Match) should write sumposite categories" $ do
            categoryToString Composite {composite_type = Match, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]} `shouldBe` "*|`a,`b|*"
        it "(Function) should write chains" $ do
            categoryToString (Composite Function [Thing (Name "a"), Composite Function [Thing (Name "b"), Thing (Name "c")]]) `shouldBe`  "`a->`b->`c"
        it "(Function) should write named morphisms" $ do
            categoryToString (Composite Function [Thing (Name "a"), Thing (Name "b")]) `shouldBe` "`a->`b"
        it "(Function) should handle nested chains" $ do
            categoryToString (Composite Function [Composite Function [Thing (Name "a"), Thing (Name "b")], Thing (Name "b")]) `shouldBe` "(`a->`b)->`b"
        it "(Placeholder) should write placeholders" $ do
            categoryToString Placeholder{name=Name "x", placeholder_kind=Variable, placeholder_category=Thing (Name "a")} `shouldBe` "x@`a"
        it "(Refinement) should write refinement" $ do
            categoryToString Refined{base=Placeholder{name=Name "x", placeholder_kind=Variable, placeholder_category=Thing (Name "a")}, predicate=Composite Function [Thing (Name "a"), Thing (Name "b")]} `shouldBe` "{x@`a | `a->`b}"
        it "(Special) should write flexible" $ do
            categoryToString Special{special_type=Flexible} `shouldBe` "(%)"
        it "(Special) should write Any" $ do
            categoryToString Special{special_type=Any} `shouldBe` "Any"
        it "(Special) should write Reference" $ do
            categoryToString Reference{name=Name "Stuff"} `shouldBe` "$Stuff"
        it "(Call) should write call" $ do
            categoryToString Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg") } `shouldBe` "$base_foo[$some_arg]"
        it "(Call) should write call consecutiveness" $ do
            categoryToString Call{base=Call{base=Reference (Name "base_foo") ,argument=Reference (Name "some_arg") },argument=Reference (Name "some_other") } `shouldBe` "$base_foo[$some_arg][$some_other]"
        it "(Dereference) should write dereferences" $ do
            categoryToString Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name"]}  `shouldBe` "($base_ref).name"
        it "(TypeAnnotation) should write membership" $ do
            categoryToString TypeAnnotation{big_category=Reference (Name "base_category"), small_category=Reference (Name "child_category") } `shouldBe` "($base_category)::($child_category)"
    describe "prettyCategoryToString" $ do
        it "Things" $ do
            prettyCategoryToString (Thing (Name "hello")) `shouldBe` "`hello"
        describe "Tuples" $ do
          it "should handle standard tuples" $ do
            -- putStrLn "(\n\t`hello,\n\t`hello2\n)"
            prettyCategoryToString (Composite Tuple [Thing (Name "hello"),Thing (Name "hello2")]) `shouldBe` "(\n\t`hello,\n\t`hello2\n)"
          it "should handle nested tuples" $ do
            -- putStrLn "(\n\t(\n\t\t`hello,\n\t\t`hello2\n\t),\n\t`hello2\n)"
            prettyCategoryToString (Composite Tuple [Composite Tuple [Thing (Name "hello"),Thing (Name "hello2")],Thing (Name "hello2")]) `shouldBe` "(\n\t(\n\t\t`hello,\n\t\t`hello2\n\t),\n\t`hello2\n)"
          it "should not newline single tuples" $ do
            prettyCategoryToString (Composite Tuple [Thing (Name "hello")]) `shouldBe` "(`hello)"
        describe "Functions" $ do
          it "should handle standard functions" $ do
            prettyCategoryToString (Composite Function [Thing (Name "a"),Thing (Name "b")]) `shouldBe` "given `a\n->\treturn `b"
            prettyCategoryToString (Composite Function [Thing (Name "a"),Thing (Name "b"), Thing (Name "c")]) `shouldBe` "given `a\n->\tgiven `b\n->\treturn `c"
          it "should handle nested composites" $ do
            prettyCategoryToString (Composite Function [Thing (Name "a"),Composite Tuple [Thing (Name "hello"),Thing (Name "hello2")]]) `shouldBe` "given `a\n->\treturn (\n\t\t`hello,\n\t\t`hello2\n\t)"
          it "should handle imports" $ do
            prettyCategoryToString (Composite Function [Import (Reference (Name "base")), Thing (Name "a")]) `shouldBe` "import $base\n->\treturn `a"