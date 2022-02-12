{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec

import CategoryData
import FrontEnds.Textual.V1.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import FrontEnds.Textual.V1.CategoryWriter
    ( categoryToText, categoryToCharList )
import Text.Megaparsec.Debug (dbg)
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryPrintableChar)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import System.Posix.Internals (puts)
import qualified Data.Text.IO as TextIO
import Debug.Trace

spec :: Spec
spec = do
    describe "Thing Parser" $ do
        it "(Things) should parse things" $ do
            parseCategoryString "`Something" `shouldBe` Thing{name=Name "Something"}
        it "(Things) should parse parenthesized things" $ do
            parseCategoryString "(`Something)" `shouldBe` Thing{name=Name "Something"}
        it "(Tuples) should parse tuples" $ do
            parseCategoryString "(`a, `b, `c)" `shouldBe` Composite {composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
        it "(Tuples) should parse recursive tuples" $ do
            parseCategoryString "(`a, (`b, `c))" `shouldBe` Composite {composition_type = Product, inner = [Thing {name = Name "a"},Composite {composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
        it "(Tuples) should parse inner morphisms" $ do
            parseCategoryString "(given `a -> return `b, given `b -> return `c)" `shouldBe` Composite {composition_type = Product, inner = [IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "b"}}]},IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "b"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "c"}}]}]}
        it "(Tuples) should handle spaces better" $ do
             parseCategoryStringWith pTuple "( `anything , `empty_list )" `shouldBe` Composite {composition_type = Product, inner = [Thing (Name "anything"), Thing (Name "empty_list")]}
        it "(Sumples) should parse sumples" $ do
            parseCategoryString "|`a, `b|" `shouldBe` Composite {composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Set) should parse set" $ do
            parseCategoryString "{`a,`b}" `shouldBe` Composite {composition_type = Set, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Higher) should parse higher categories" $ do
            parseCategoryString "^{`a,`b}" `shouldBe` Composite {composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Morphism) should parse morphisms" $ do
            parseCategoryString "given `a -> return `b" `shouldBe` IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "b"}}]}
        it "(Morphism) should parse chains" $ do
            parseCategoryString  "given `a -> given `b -> return `c" `shouldBe` IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Given, m_category = Thing {name = Name "b"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "c"}}]}
        it "(Morphism) should parse named morphisms" $ do
            parseCategoryString "AB:given `a -> return `b" `shouldBe` Label {name = Name "AB", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "b"}}]}}
        it "(Composition) should parse compositions" $ do
            let a_to_b = parseCategoryString "given `a -> return `b"
            let b_to_c = parseCategoryString "given `b -> return `c"
            parseCategoryString "*(given `a -> return `b, given `b -> return `c)*" `shouldBe` Composite{composition_type=Composition,inner=[a_to_b, b_to_c]}
        it "(Sumposition) should parse sumpositions" $ do
            let a_to_b = parseCategoryString "given `a -> return `b"
            let b_to_c = parseCategoryString "given `b -> return `c"
            parseCategoryString "*|given `a -> return `b, given `b -> return `c|" `shouldBe` Composite{composition_type=Sumposition,inner=[a_to_b, b_to_c]}
        it "(Placeholder) should parse placeholders" $ do
            parseCategoryString "x@`a" `shouldBe` Placeholder{name=Name "x", ph_level=AnyLevel, ph_category=Thing (Name "a")}
            parseCategoryString "x@(`a)" `shouldBe` Placeholder{name=Name "x", ph_level=AnyLevel, ph_category=Thing (Name "a")}
            parseCategoryString "x<1>@`a" `shouldBe` Placeholder{name=Name "x", ph_level=Specific 1, ph_category=Thing (Name "a")}
            parseCategoryString "x<1>@(`a)" `shouldBe` Placeholder{name=Name "x", ph_level=Specific 1, ph_category=Thing (Name "a")}
            parseCategoryString "@`a" `shouldBe` Placeholder{name=Unnamed, ph_level=AnyLevel, ph_category=Thing (Name "a")}
            parseCategoryString "<1>@`a" `shouldBe` Placeholder{name=Unnamed, ph_level=Specific 1, ph_category=Thing (Name "a")}
        it "(Refinement) should parse refinement" $ do
            parseCategoryString "{x@(`a) | given `a -> return `b}" `shouldBe` RefinedCategory {base_category = Placeholder {name = Name "x", ph_level = AnyLevel, ph_category = Thing {name = Name "a"}}, predicate = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "b"}}]}}
        it "(Special) should parse flexible" $ do
            parseCategoryString "(%)" `shouldBe` Special{special_type=Flexible}
            parseCategoryString "test:(%)" `shouldBe` CategoryData.Label {name = Name "test", target = Special {special_type = Flexible}}
        it "(Special) should parse Universal" $ do
            parseCategoryString "Any" `shouldBe` Special{special_type=Universal}
            parseCategoryString "test:Any" `shouldBe` CategoryData.Label {name = Name "test", target = Special {special_type = Universal}}
        it "(Special) should parse Reference" $ do
            parseCategoryString "$Stuff" `shouldBe` Reference (Name "Stuff")
        it "(Call) should parse call" $ do
            parseCategoryString "$base_foo[$some_arg]" `shouldBe` MorphismCall{base_morphism=Reference (Name "base_foo"),argument=Reference (Name "some_arg")}
        it "(Call) should parse tuple call" $ do
            parseCategoryString "tail: $List[$list_type]" `shouldBe` Label {name = Name "tail", target = MorphismCall {base_morphism = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}
        it "(Call) should call consecutiveness" $ do
            parseCategoryString "$base_foo[$some_arg][$some_other]" `shouldBe` MorphismCall{base_morphism=MorphismCall{base_morphism=Reference (Name "base_foo"),argument=Reference (Name "some_arg")},argument=Reference (Name "some_other") }
        it "(Dereference) should parse dereferences" $ do
            parseCategoryString "$base_ref.name" `shouldBe` Dereference{base_category=Reference{name=Name "base_ref"}, category_id=Name "name"}
            parseCategoryString "$base_ref.name.name" `shouldBe` Dereference{base_category=Dereference{base_category=Reference{name=Name "base_ref"}, category_id=Name "name"}, category_id=Name "name"}
        it "(Membership) should parse membership" $ do
            parseCategoryString "$base_category::$child_category" `shouldBe` Membership{big_category=Reference (Name "base_category") , small_category=Reference (Name "child_category") }
        it "(Recursive) should parse recursion" $ do
            parseCategoryString "self:(given `a -> return $self[`b])" `shouldBe`  Label {name = Name "self", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "a"}},MorphismTerm {m_type = Return, m_category = MorphismCall {base_morphism = Reference {name = Name "self"}, argument = Thing {name = Name "b"}}}]}}
            