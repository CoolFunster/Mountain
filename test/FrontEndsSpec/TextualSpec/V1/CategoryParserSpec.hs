{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec

import CategoryData
import FrontEnds.Textual.V1.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight)

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
            parseCategoryString "(`Something)" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "Something"}]}
        it "(Tuples) should parse tuples" $ do
            parseCategoryString "(`a, `b, `c)" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
        it "(Tuples) should parse recursive tuples" $ do
            parseCategoryString "(`a, (`b, `c))" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
        it "(Morphism) should parse morphisms" $ do
            parseCategoryString "`a -> `b" `shouldBe` Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}
        it "(Morphism) should parse chains" $ do
            parseCategoryString  "`a -> `b -> `c" `shouldBe` Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b"), Thing (Name "c")]}
        it "(Morphism) should parse named morphisms" $ do
            parseCategoryString "AB:(`a -> `b)" `shouldBe` Placeholder {name = Name "AB", placeholder_type = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]}}
        it "(Morphism) should not catch label" $ do
            parseCategoryString "AB:`a -> `b" `shouldBe` Composite {composite_type = Function, inner_categories = [Placeholder {name = Name "AB", placeholder_type = Label, placeholder_category = Thing {name = Name "a"}},Thing {name = Name "b"}]}
        it "(Tuples) should parse inner morphisms" $ do
            parseCategoryString "(`a -> `b, `b -> `c)" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Composite {composite_type=Function, inner_categories=[Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type=Function, inner_categories=[Thing (Name "b"), Thing (Name "c")]}]}
        it "(Tuples) should handle spaces better" $ do
             parseCategoryString "( `anything , `empty_list )" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing (Name "anything"), Thing (Name "empty_list")]}
        it "(Sumples) should parse sumples" $ do
            parseCategoryString "|`a, `b|" `shouldBe` Composite {composite_type = Sumple, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Set) should parse set" $ do
            parseCategoryString "{`a,`b}" `shouldBe` Composite {composite_type = Set, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Composition) should parse compositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*(`a -> `b, `b -> `c)*" `shouldBe` Composite{composite_type=Composition,inner_categories=[a_to_b, b_to_c]}
        it "(Case) should parse sumpositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*|`a -> `b, `b -> `c|*" `shouldBe` Composite{composite_type=Case,inner_categories=[a_to_b, b_to_c]}
        it "(Placeholder) should parse placeholders" $ do
            parseCategoryString "x@`a" `shouldBe` Placeholder{name=Name "x", placeholder_type=Element, placeholder_category=Thing (Name "a")}
            parseCategoryString "x@(`a)" `shouldBe` Placeholder {name = Name "x", placeholder_type = Element, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}
            parseCategoryString "@`a" `shouldBe` Placeholder{name=Unnamed, placeholder_type=Element, placeholder_category=Thing (Name "a")}
        it "(Refinement) should parse refinement" $ do
            parseCategoryString "{x@(`a) | `a -> `b}" `shouldBe` Refined {base = Placeholder {name = Name "x", placeholder_type = Element, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}, predicate = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}}
        it "(Special) should parse flexible" $ do
            parseCategoryString "(%)" `shouldBe` Special{special_type=Flexible}
            parseCategoryString "test:(%)" `shouldBe` Placeholder {name = Name "test", placeholder_type=Label, placeholder_category = Special {special_type = Flexible}}
        it "(Special) should parse Universal" $ do
            parseCategoryString  "Any" `shouldBe` Special{special_type=Universal}
            parseCategoryString "test:Any" `shouldBe` Placeholder {name = Name "test", placeholder_type=Label, placeholder_category = Special {special_type = Universal}}
        it "(Special) should parse Reference" $ do
            parseCategoryString "$Stuff" `shouldBe` Reference (Name "Stuff")
        it "(Call) should parse call" $ do
            parseCategoryString "$base_foo[$some_arg]" `shouldBe` FunctionCall{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")}
        it "(Call) should parse tuple call" $ do
            parseCategoryString "tail: $List[$list_type]" `shouldBe` Placeholder {name = Name "tail", placeholder_type=Label, placeholder_category = FunctionCall {base = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}
        it "(Call) should call consecutiveness" $ do
            parseCategoryString "$base_foo[$some_arg][$some_other]" `shouldBe` FunctionCall{base=FunctionCall{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")},argument=Reference (Name "some_other") }
        it "(Dereference) should parse dereferences" $ do
            parseCategoryString "$base_ref.name" `shouldBe` Access{base=Reference{name=Name "base_ref"}, access_id=Name "name"}
            parseCategoryString "$base_ref.name.name" `shouldBe` Access{base=Access{base=Reference{name=Name "base_ref"}, access_id=Name "name"}, access_id=Name "name"}
        it "(Membership) should parse membership" $ do
            parseCategoryString "$base_category::$child_category" `shouldBe` Membership{big_category=Reference (Name "base_category") , small_category=Reference (Name "child_category") }
        it "(Recursive) should parse recursion" $ do
            parseCategoryString "self:(`a -> $self[`b])" `shouldBe`  Placeholder {name = Name "self", placeholder_type = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},FunctionCall {base = Reference {name = Name "self"}, argument = Thing {name = Name "b"}}]}]}}
    -- describe "Module loader" $ do
    --     it "should load files" $ do
    --         result <- loadModule "base.list"
    --         result `shouldBe` result
    --     it "should load directories" $ do
    --         result <- loadModule "base"
    --         result `shouldBe` trace (categoryToStr result) result
            