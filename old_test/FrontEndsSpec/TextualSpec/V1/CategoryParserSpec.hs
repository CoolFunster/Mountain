{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec

import Category
import FrontEnds.Textual.V1.Mountain
import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter

import Data.Maybe (fromJust)
import Data.Either (fromRight, isRight, isLeft, fromLeft)

import Text.Megaparsec.Debug (dbg)
import System.Posix.Internals (puts)
import qualified Data.Text.IO as TextIO
import Debug.Trace

parseTest :: String -> Category -> Bool
parseTest s c = do
  let result = parseCategoryString s
  result == Right c

spec :: Spec
spec = do
  describe "Thing Parser" $ do
    it "(Things) should parse things" $ do
      let result = parseCategoryString "#Something"
      shouldBe result $ Right $ Thing{name=Name "Something"}
    it "(Things) should parse parenthesized things" $ do
      let result = parseCategoryString "(#Something)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "Something"}]}
    it "(Things) should not parse unnamed things" $ do
      let result = parseCategoryString "#_"
      result `shouldSatisfy` isLeft
    it "(Tuples) should parse tuples" $ do
      let result = parseCategoryString "(#a, #b, #c)"
      shouldBe result $ Right $ Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
    it "(Tuples) should parse recursive tuples" $ do
      let result = parseCategoryString "(#a, (#b, #c))"
      shouldBe result $ Right $ Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
    it "(Tuples) should parse inner morphisms" $ do
      let result = parseCategoryString "(#a -> #b, #b -> #c)"
      shouldBe result $ Right $ Composite {composite_type = Tuple, inner_categories = [Composite {composite_type=Function, inner_categories=[Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type=Function, inner_categories=[Thing (Name "b"), Thing (Name "c")]}]}
    it "(Tuples) should handle spaces better" $ do
      let result = parseCategoryString "( #anything , #empty_list )"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Tuple, inner_categories = [Thing (Name "anything"), Thing (Name "empty_list")]}
    it "(Tuples) should parse inner function calls" $ do
      let result = parseCategoryString "(#anything #empty_list)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Tuple, inner_categories = [Call {base = Thing {name = Name "anything"}, argument = Thing {name = Name "empty_list"}}]}
    it "(Tuples) should allow trailing comma" $ do
      let result = parseCategoryString "(#anything, #empty_list,)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "anything"},Thing {name = Name "empty_list"}]}
    it "(Function) should parse morphisms" $ do
      let result = parseCategoryString "#a -> #b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}
    it "(Function) should parse chains" $ do
      let result = parseCategoryString "#a -> #b -> #c"
      shouldBe result $ Right $ Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b"), Thing (Name "c")]}
    it "(Function) should parse labeled function as label of function" $ do
      let result = parseCategoryString "AB:#a -> #b"
      shouldBe result $ Right (Placeholder {name = Name "AB", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}})
    it "(Function) should need parenthesis for specific var labels" $ do
      let result = parseCategoryString "(AB:#a) -> #b"
      shouldBe result $ Right (Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "AB", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}}]},Thing {name = Name "b"}]})
    it "(Function) should parse function calls" $ do
      let result = parseCategoryString "(c, _, _) -> sc.print (c, _)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}]},Call {base = Access {base = Reference {name = Name "sc"}, access_type = ByLabelGroup [Name "print"]}, argument = Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}]}}]}
    it "(Function) should parse chained function calls" $ do
      let result = parseCategoryString "(c, _, _) -> sc.print (c, _) -> x"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}]},Call {base = Access {base = Reference {name = Name "sc"}, access_type = ByLabelGroup [Name "print"]}, argument = Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}]}},Reference {name = Name "x"}]}
    it "(Function) should parse chained function calls 2" $ do
      let result = parseCategoryString "a b -> a b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Call {base = Reference {name = Name "a"}, argument = Reference {name = Name "b"}},Call {base = Reference {name = Name "a"}, argument = Reference {name = Name "b"}}]}

    it "(Eithers) should parse sumples" $ do
      let result = parseCategoryString "*|#a, #b|*"
      shouldBe result $ Right $ Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}
    it "(Eithers) should parse sumple of a function" $ do
      let result = parseCategoryString "*|a -> b|*"
      shouldBe result $ Right (Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Reference {name = Name "a"},Reference {name = Name "b"}]}]})
    it "(Eithers) should parse nested eithers" $ do
      let result = parseCategoryString "*|a -> *|a,b|*|*"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Reference {name = Name "a"},Composite {composite_type = Either, inner_categories = [Reference {name = Name "a"},Reference {name = Name "b"}]}]}]}
    it "(Composition) should parse compositions" $ do
      let result = parseCategoryString "(#a -> #b;  #b -> #c)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite{composite_type=Composition,inner_categories=[Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type = Function, inner_categories = [Thing (Name "b"), Thing (Name "c")]}]}
    it "(Match) should parse sumpositions" $ do
      let result = parseCategoryString "*|#a -> #b; #b -> #c|*"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite{composite_type=Match,inner_categories=[Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type = Function, inner_categories = [Thing (Name "b"), Thing (Name "c")]}]}
      let result = parseCategoryString "*|(#a -> #b); (#b -> #c)|*"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}
    it "(Placeholder) should parse placeholders" $ do
      let result = parseCategoryString "#a::x"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder{name=Name "x", placeholder_kind=Variable, placeholder_category=Thing (Name "a")}
    it "(Placeholder) should parse labels" $ do
      let result = parseCategoryString "x:#a"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder{name=Name "x", placeholder_kind=Label, placeholder_category=Thing (Name "a")}
    it "(Placeholder) should parse nested labels" $ do
      let result = parseCategoryString "x:(x:#a)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}}]}}
    it "(Refinement) should parse refinement" $ do
      let result = parseCategoryString "{#a::x | #a -> #b}"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Refined {base = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Thing {name = Name "a"}}, predicate = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}}
    it "(Special) should parse flexible" $ do
      let result = parseCategoryString "?"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Special {special_type = Flexible}
    it "(Special) should parse Any" $ do
      let result = parseCategoryString "Any"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Special{special_type=Any}
    it "(Reference) should parse Reference" $ do
      let result = parseCategoryString "$Stuff"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Reference (Name "Stuff")
      let result = parseCategoryString "Stuff"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Reference (Name "Stuff")
    it "(Call) should parse call" $ do
      let result = parseCategoryString "base_foo some_arg"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")}
    it "(Call) should parse tuple call" $ do
      let result = parseCategoryString "tail:(List list_type)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Call {base = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}]}}
    it "(Call) should call consecutiveness" $ do
      let result = parseCategoryString "base_foo some_arg some_other"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call{base=Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")},argument=Reference (Name "some_other") }
    it "(Call) should call tuples" $ do
      let result = parseCategoryString "(base_foo some_arg)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Tuple, inner_categories = [Call {base = Reference {name = Name "base_foo"}, argument = Reference {name = Name "some_arg"}}]}
    it "(Call) should call tuples 2" $ do
      let result = parseCategoryString "base_foo (some_arg)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call {base = Reference {name = Name "base_foo"}, argument = Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "some_arg"}]}}
    it "(Call) should handle infix" $ do
      let result = parseCategoryStringWith pCall "a `plus b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call {base = Call {base = Reference {name = Name "plus"}, argument = Reference {name = Name "a"}}, argument = Reference {name = Name "b"}}
    it "(Call) should handle chained infix" $ do
      let result = parseCategoryStringWith pCall "a `plus b `plus c"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call {base = Call {base = Reference {name = Name "plus"}, argument = Call {base = Call {base = Reference {name = Name "plus"}, argument = Reference {name = Name "a"}}, argument = Reference {name = Name "b"}}}, argument = Reference {name = Name "c"}}
    it "(Call) should handle mixed infix" $ do
      let result = parseCategoryStringWith pCall "a b c `plus d"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call {base = Call {base = Reference {name = Name "plus"}, argument = Call {base = Call {base = Reference {name = Name "a"}, argument = Reference {name = Name "b"}}, argument = Reference {name = Name "c"}}}, argument = Reference {name = Name "d"}}
    it "(Dereference) should parse dereferences" $ do
      let result = parseCategoryString "$base_ref.name"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name"]}
    it "(Access) should parse dot access" $ do
      let result = parseCategoryString "base_ref.name"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name"]}
    it "(Access) should parse bracket" $ do
      let result = parseCategoryString "base_ref.[name]"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name"]}
    it "(Access) should parse multiple bracket" $ do
      let result = parseCategoryString "base_ref.[name, a , b]"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name", Name "a", Name "b"]}
    it "(Access) should parse subtractive bracket" $ do
      let result = parseCategoryString "base_ref.-[name, a , b]"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=Subtractive [Name "name", Name "a", Name "b"]}
    it "(Recursive) should parse recursion" $ do
      let result = parseCategoryString "self:(#a -> self #b)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder {name = Name "self", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Call {base = Reference {name = Name "self"}, argument = Thing {name = Name "b"}}]}]}}
    it "(Open Scope) shoud parse multiple statements" $ do
      let example = "x: #a; y: #b; return (first: x,second: y)"
      let result = parseCategoryStringWith pOpenScope example
      case result of
        Left e -> error e
        Right cat -> do
          cat `shouldBe` Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "first", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}},Placeholder {name = Name "second", placeholder_kind = Label, placeholder_category = Reference {name = Name "y"}}]}]}
          res <- getResultOfT $ execute strict cat
          fromRight (error "404") res `shouldBe` Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "first", placeholder_kind = Label, placeholder_category = Placeholder {name = Name "x", placeholder_kind = Resolved, placeholder_category = Thing {name = Name "a"}}},Placeholder {name = Name "second", placeholder_kind = Label, placeholder_category = Placeholder {name = Name "y", placeholder_kind = Resolved, placeholder_category = Thing {name = Name "b"}}}]}
    it "(Import) should parse import" $ do
      let result = parseCategoryString "import test.test1"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Import {import_category = Access {base = Reference {name = Name "test"}, access_type =ByLabelGroup [Name "test1"]}}
    it "(Import) should not be greedy" $ do
      let result = parseCategoryString "import test.test1 -> #a"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Import {import_category = Access {base = Reference {name = Name "test"}, access_type = ByLabelGroup [Name "test1"]}},Thing {name = Name "a"}]}
    it "(TypeAnnotation) should parse type annotation" $ do
      let result = parseCategoryString "#a::#b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ TypeAnnotation {big_category = Thing {name = Name "a"}, small_category = Thing {name = Name "b"}}
    it "(TypeAnnotation) should independent import type annotation" $ do
      let result = parseCategoryString "import a -> #a::#b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Import {import_category = Reference {name = Name "a"}},TypeAnnotation {big_category = Thing {name = Name "a"}, small_category = Thing {name = Name "b"}}]}
    describe "File loading" $ do
      it "should load files - test1" $ do
        result <- getResultOfT $ loadTextual "Tests.test1"
        result `shouldBe` Right (Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}}]},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "a", placeholder_kind = Label, placeholder_category = Thing {name = Name "1"}},Placeholder {name = Name "b", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}}]}]}]}})
      it "should load files - test2" $ do
        result <- getResultOfT $ loadTextual "Tests.test2"
        result `shouldBe` Right (Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}})
    describe "Scope Parsing" $ do
      it "(Scope) should parse simple scopes" $ do
        let result = parseCategoryString "*{x:#something; x}*"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}},Reference {name = Name "x"}]}
      it "should parse simple scopes with return" $ do
        let result = parseCategoryString "*{x:#something; return x}*"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}},Reference {name = Name "x"}]}
      it "should parse long scopes with non bindings" $ do
        let result = parseCategoryString "*{x: #something; return x}*"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}},Reference {name = Name "x"}]}
    describe "Parse Category File" $ do
      it "should parse test 3" $ do
        result <- parseCategoryFile (textualBasePath ++ "Tests/test3.mtn")
        -- print result
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "first", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}},Placeholder {name = Name "second", placeholder_kind = Label, placeholder_category = Reference {name = Name "y"}}]}]}
    describe "Example Loading" $ do
      it "should parse Hello World" $ do
        result <- getResultOfT $ loadTextual "Examples.HelloWorld"
        case result of
          Left err -> error (show err)
          Right something -> shouldBe something $ Placeholder {name = Name "HelloWorld", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Import {import_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "sc", placeholder_kind = Label, placeholder_category = Access {base = Reference {name = Name "Body"}, access_type = ByLabelGroup [Name "SimpleConsole"]}}]}},Scope {statements = [Binding {placeholder = Placeholder {name = Name "HelloWorld", placeholder_kind = Variable, placeholder_category = Access {base = Reference {name = Name "sc"}, access_type = ByLabelGroup [Name "ConsoleProgram"]}}, category_to_bind = Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Reference {name = Name "argc"},Reference {name = Name "argv"}]},Call {base = Access {base = Reference {name = Name "sc"}, access_type = ByLabelGroup [Name "print"]}, argument = Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "c"},Placeholder {name = Unnamed, placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}]}}]}},Reference {name = Name "HelloWorld"}]}]}}
      it "should parse Linked List" $ do
        result <- getResultOfT $ loadTextual "Base.Data.Basic.linkedList"
        case result of
          Left err -> error (show err)
          Right something -> do
            -- let res = fromRight (error "404") $ getResultOf $ evaluateAccess $ Access something (ByLabelGroup [Name "forl2r"])
            -- putStrLn $ prettyCategoryToString res
            -- shouldBe something $ Placeholder {name = Name "linkedList", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "LinkedList", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "ListType", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Either, inner_categories = [Placeholder {name = Name "empty", placeholder_kind = Label, placeholder_category = Set {elements = [Thing {name = Name "empty"}]}},Placeholder {name = Name "nonempty", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Reference {name = Name "ListType"}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Call {base = Reference {name = Name "LinkedList"}, argument = Reference {name = Name "ListType"}}}]}}]}]}},Placeholder {name = Name "push", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "Vt", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Call {base = Reference {name = Name "LinkedList"}, argument = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "Lt", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]}},Call {base = Reference {name = Name "LinkedList"}, argument = Composite {composite_type = Tuple, inner_categories = [Call {base = Call {base = Reference {name = Name "Vt"}, argument = Reference {name = Name "union"}}, argument = Reference {name = Name "Lt"}}]}}]}]}, small_category = Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "new_head", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "old_list", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "new_head"},Reference {name = Name "old_list"}]}]}]}}},Placeholder {name = Name "pop", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Access {base = Composite {composite_type = Tuple, inner_categories = [Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}}]}, access_type = ByLabelGroup [Name "nonempty"]},Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}}]}]}, small_category = Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Special {special_type = Flexible},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Reference {name = Name "tail"}]}]}}},Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}},Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}}]}]}, small_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "empty"},Thing {name = Name "empty"}]},Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}},Special {special_type = Flexible}]},Reference {name = Name "head"}]}]}}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Access {base = Composite {composite_type = Tuple, inner_categories = [Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}}]}, access_type = ByLabelGroup [Name "nonempty"]},Call {base = Reference {name = Name "LinkedList"}, argument = Special {special_type = Flexible}}]}]}, small_category = Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Special {special_type = Flexible},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Reference {name = Name "tail"}]}]}}},Placeholder {name = Name "map", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "A", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "B", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]}]}]},Call {base = Reference {name = Name "LinkedList"}, argument = Reference {name = Name "A"}},Call {base = Reference {name = Name "LinkedList"}, argument = Reference {name = Name "B"}}]}]}, small_category = Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "map_foo", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Match, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "empty"},Thing {name = Name "empty"}]},Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Tuple, inner_categories = [Call {base = Reference {name = Name "map_foo"}, argument = Reference {name = Name "head"}},Call {base = Call {base = Reference {name = Name "map"}, argument = Reference {name = Name "map_foo"}}, argument = Reference {name = Name "tail"}}]}]}]}]}]}}},Placeholder {name = Name "forl2r", placeholder_kind = Label, placeholder_category = TypeAnnotation {big_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "StateType", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Call {base = Reference {name = Name "LinkedList"}, argument = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "Lt", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]}},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Reference {name = Name "StateType"},Call {base = Reference {name = Name "LinkedList"}, argument = Reference {name = Name "Lt"}},Reference {name = Name "StateType"}]}]},Reference {name = Name "StateType"}]}]}, small_category = Composite {composite_type = Either, inner_categories = [Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "state", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Match, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "empty"},Special {special_type = Flexible},Thing {name = Name "empty"}]},Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "for_body", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Scope {statements = [Placeholder {name = Name "new_state", placeholder_kind = Label, placeholder_category = Call {base = Call {base = Reference {name = Name "for_body"}, argument = Reference {name = Name "state"}}, argument = Reference {name = Name "head"}}},Composite {composite_type = Tuple, inner_categories = [Reference {name = Name "new_state"},Call {base = Call {base = Call {base = Reference {name = Name "forl2r"}, argument = Reference {name = Name "state"}}, argument = Reference {name = Name "tail"}}, argument = Reference {name = Name "for_body"}}]}]}]}]}]}]}}}]}}
            something `shouldBe` something
    describe "Parse & Execute" $ do
      let executePlain = execute (Options{reduce_composite=True, importer=loadTextual})
      let parseAndExecute str = do {
        let result = parseCategoryString str
        ;(case result of
          Left e -> error e
          Right cat -> executePlain cat)
      }
      it "(Tuple) should parse and return simple tuple" $ do
        result <- getResultOfT $ parseAndExecute "(#a, #b)"
        shouldBe result $ Right (Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]})
      it "(Import) should parse and return basic import" $ do
        result <- getResultOfT $ parseAndExecute "import Base.Data.Basic.Bool -> Bool.Bool"
        shouldBe result $ Right (Placeholder {name = Name "Bool", placeholder_kind = Label, placeholder_category = Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]}})
      it "(Import) should parse and return statement sequence" $ do
        result <- getResultOfT $ evaluateImport loadTextual (Import $ Access{base=(Reference (Name "Tests")), access_type=(ByLabelGroup [Name "test3"])})
        -- result <- getResultOfT $ parseAndExecute "import (t:Tests.test3) -> t"
        shouldBe result $ Right (Scope {statements = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "first", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}},Placeholder {name = Name "second", placeholder_kind = Label, placeholder_category = Reference {name = Name "y"}}]}]})
      it "(Import) should parse and return labeled import" $ do
        result <- getResultOfT $ parseAndExecute "import (b:Base.Data.Basic.Bool) -> b.Bool"
        shouldBe result $ Right (Placeholder {name = Name "Bool", placeholder_kind = Label, placeholder_category = Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]}})
      it "(Import) should parse and handle dirs" $ do
        result <- getResultOfT $ parseAndExecute "import (b:Tests) -> b"
        shouldBe result $ Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "Tests.test1"}}},Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "Tests.test2"}}},Placeholder {name = Name "test3", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "Tests.test3"}}}]})
      it "(Import) should handle stars" $ do
        result <- getResultOfT $ parseAndExecute "import (*:Base.Data.Basic.Bool) -> Bool"
        shouldBe result $ Right (Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]})
      it "(Import) should handle multiple" $ do
        result <- getResultOfT $ parseAndExecute "import (x:Base.Data.Basic.Bool, b: Base.Data.Basic.Maybe) -> (x.Bool, b #a)"
        shouldBe result $ Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "Bool", placeholder_kind = Label, placeholder_category = Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]}},Composite {composite_type = Either, inner_categories = [Thing {name = Name "Nothing"},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "Something"},Thing {name = Name "a"}]}]}]})
      it "(Import) should handle multiple star" $ do
        result <- getResultOfT $ parseAndExecute "import (*:Base.Data.Basic.Bool, *: Base.Data.Basic.Char) -> (Bool, Whitespace)"
        shouldBe result $ Right (Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]},Special {special_type = Flexible}]})
      it "(Import) should handle star with dup name" $ do
        result <- getResultOfT $ parseAndExecute "import (a:Base.Data.Basic.Bool, b: Base.Data.Basic.Maybe) -> (a.Bool, b #a)"
        shouldBe result $ Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "Bool", placeholder_kind = Label, placeholder_category = Composite {composite_type = Either, inner_categories = [Thing {name = Name "True"},Thing {name = Name "False"}]}},Composite {composite_type = Either, inner_categories = [Thing {name = Name "Nothing"},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "Something"},Thing {name = Name "a"}]}]}]})
      it "(Import) should err on non existence" $ do
        result <- getResultOfT $ parseAndExecute "import bababa"
        print result
        shouldBe (isLeft result) $ True
        error_type (head $ fromLeft (error "404") result) `shouldBe` BadImport
      it "(Import) should err on sub non existence" $ do
        result <- getResultOfT $ parseAndExecute "import (b:Base.bababa) -> b"
        shouldBe (isLeft result) $ True
        error_type (head $ fromLeft (error "404") result) `shouldBe` BadImport
        -- shouldBe (isLeft result) $ True