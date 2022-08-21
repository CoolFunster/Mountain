{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec

import Category
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
    let executeTextual = fmap fst . runCategoryContextT . execute (Options{reduce_composite=False, importer=loadTextual})
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
            parseCategoryString "AB:(`a -> `b)" `shouldBe` Variable {name = Name "AB", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]}}
        it "(Morphism) should not catch label" $ do
            parseCategoryString "AB:`a -> `b" `shouldBe` Composite {composite_type = Function, inner_categories = [Variable {name = Name "AB", variable_kind = Label, variable_category = Thing {name = Name "a"}},Thing {name = Name "b"}]}
        it "(Tuples) should parse inner morphisms" $ do
            parseCategoryString "(`a -> `b, `b -> `c)" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Composite {composite_type=Function, inner_categories=[Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type=Function, inner_categories=[Thing (Name "b"), Thing (Name "c")]}]}
        it "(Tuples) should handle spaces better" $ do
             parseCategoryString "( `anything , `empty_list )" `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing (Name "anything"), Thing (Name "empty_list")]}
        it "(Eithers) should parse sumples" $ do
            parseCategoryString "|`a, `b|" `shouldBe` Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Composition) should parse compositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*(`a -> `b, `b -> `c)*" `shouldBe` Composite{composite_type=Composition,inner_categories=[a_to_b, b_to_c]}
        it "(Match) should parse sumpositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*|`a -> `b, `b -> `c|*" `shouldBe` Composite{composite_type=Match,inner_categories=[a_to_b, b_to_c]}
        it "(Variable) should parse placeholders" $ do
            parseCategoryString "x@`a" `shouldBe` Variable{name=Name "x", variable_kind=Element, variable_category=Thing (Name "a")}
            parseCategoryString "x@(`a)" `shouldBe` Variable {name = Name "x", variable_kind = Element, variable_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}
        it "(Variable) should parse labels" $ do
            parseCategoryString "x:`a" `shouldBe` Variable{name=Name "x", variable_kind=Label, variable_category=Thing (Name "a")}
            parseCategoryString "x:(`a)" `shouldBe` Variable {name = Name "x", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}
        it "(Variable) should parse nested labels" $ do
            parseCategoryString "x:(x:`a)" `shouldBe` Variable {name = Name "x", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "x", variable_kind = Label, variable_category = Thing {name = Name "a"}}]}}
        it "(Refinement) should parse refinement" $ do
            parseCategoryString "{x@(`a) | `a -> `b}" `shouldBe` Refined {base = Variable {name = Name "x", variable_kind = Element, variable_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}, predicate = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}}
        it "(Special) should parse flexible" $ do
            parseCategoryString "(%)" `shouldBe` Special{special_type=Flexible}
            parseCategoryString "test:(%)" `shouldBe` Variable {name = Name "test", variable_kind=Label, variable_category = Special {special_type = Flexible}}
        it "(Special) should parse Any" $ do
            parseCategoryString  "Any" `shouldBe` Special{special_type=Any}
            parseCategoryString "test:Any" `shouldBe` Variable {name = Name "test", variable_kind=Label, variable_category = Special {special_type = Any}}
        it "(Special) should parse Reference" $ do
            parseCategoryString "$Stuff" `shouldBe` Reference (Name "Stuff")
        it "(Call) should parse call" $ do
            parseCategoryString "$base_foo[$some_arg]" `shouldBe` Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")}
        it "(Call) should parse tuple call" $ do
            parseCategoryString "tail: $List[$list_type]" `shouldBe` Variable {name = Name "tail", variable_kind=Label, variable_category = Call {base = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}
        it "(Call) should call consecutiveness" $ do
            parseCategoryString "$base_foo[$some_arg][$some_other]" `shouldBe` Call{base=Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")},argument=Reference (Name "some_other") }
        it "(Dereference) should parse dereferences" $ do
            parseCategoryString "$base_ref.name" `shouldBe` Access{base=Reference{name=Name "base_ref"}, access_id=Name "name"}
            parseCategoryString "$base_ref.name.name" `shouldBe` Access{base=Access{base=Reference{name=Name "base_ref"}, access_id=Name "name"}, access_id=Name "name"}
        it "(TypeAnnotation) should parse membership" $ do
            parseCategoryString "$base_category::$child_category" `shouldBe` TypeAnnotation{big_category=Reference (Name "base_category") , small_category=Reference (Name "child_category") }
        it "(Recursive) should parse recursion" $ do
            parseCategoryString "self:(`a -> $self[`b])" `shouldBe`  Variable {name = Name "self", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Call {base = Reference {name = Name "self"}, argument = Thing {name = Name "b"}}]}]}}
        it "(Import) should parse import" $ do
            parseCategoryString "import $test.test1" `shouldBe` Import {import_category = Access {base = Reference {name = Name "test"}, access_id = Name "test1"}}
        it "(Define) should parse define" $ do
            parseCategoryString "define x:`something" `shouldBe` Definition{def_category=Variable{name=Name "x", variable_kind=Label, variable_category=Thing (Name "something")}}
    describe "File loading" $ do
        it "should load files - test1" $ do
            result <- executeTextual (Import (Reference (Name "test.test1")))
            result `shouldBe` Right (Variable {name = Name "test1", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Variable {name = Name "x", variable_kind = Label, variable_category = Thing {name = Name "something"}},Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "a", variable_kind = Label, variable_category = Thing {name = Name "1"}},Variable {name = Name "b", variable_kind = Label, variable_category = Reference {name = Name "x"}}]}]}]}})
        it "should load files - test2" $ do
            result <- executeTextual (Import (Reference (Name "test.test2")))
            result `shouldBe` Right (Variable {name = Name "test2", variable_kind = Label, variable_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}})
        it "should load files - test3" $ do
            result <- executeTextual (Import (Reference (Name "test.test_define")))
            result `shouldBe`  Right (Variable {name = Name "test_define", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Definition {def_category = Variable {name = Name "x", variable_kind = Label, variable_category = Thing {name = Name "something"}}},Reference {name = Name "x"}]}]}})
        it "should load dirs" $ do
            result <- executeTextual (Import (Reference (Name "test")))
            result `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "test1", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test1"}}},Variable {name = Name "test2", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test2"}}},Variable {name = Name "test_define", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test_define"}}}]})
        it "should load tuples" $ do
            result <- executeTextual $ Import (Composite Tuple [Reference (Name "test.test1"), Reference (Name "test.test2"), Reference (Name "test.test_define")])
            result1 <- executeTextual (Import (Reference (Name "test.test1")))
            result2 <- executeTextual (Import (Reference (Name "test.test2")))
            result3 <- executeTextual (Import (Reference (Name "test.test_define")))
            result `shouldBe` Right (Composite Tuple (map (fromRight (error "404")) [result1, result2, result3]))
        it "should load placeholders" $ do
            result <- executeTextual $ Import (Variable (Name "some_name") Label (Reference (Name "test.test1")))
            result1 <- executeTextual (Import (Reference (Name "test.test1")))
            result `shouldBe` Right (Variable (Name "some_name") Label (variable_category (fromRight (error "404") result1)))
    describe "executeTextual" $ do
        it "should evaluate import" $ do
            let parsed_result = parseCategoryString "import $test"
            result <- executeTextual parsed_result
            result `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "test1", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test1"}}},Variable {name = Name "test2", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test2"}}},Variable {name = Name "test_define", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test_define"}}}]})
        it "should evaluate indexed imports" $ do
            let parsed_result = parseCategoryString "import $test.test1"
            result <- executeTextual parsed_result
            result1 <- executeTextual (Import (Reference (Name "test.test1")))
            result `shouldBe` result1
        it "should evaluate labeled imports" $ do
            let parsed_result = parseCategoryString "import x:$test.test1"
            result <- executeTextual parsed_result
            result1 <- executeTextual (Import (Reference (Name "test.test1")))
            result `shouldBe` Right (Variable {name = Name "x", variable_kind = Label, variable_category = (variable_category . fromRight (error "404")) result1 })