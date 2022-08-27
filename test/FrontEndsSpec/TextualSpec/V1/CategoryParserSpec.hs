{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec

import Category
import FrontEnds.Textual.V1.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight, isRight, isLeft)

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
    it "(Morphism) should parse morphisms" $ do
      let result = parseCategoryString "#a -> #b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}
    it "(Morphism) should parse chains" $ do
      let result = parseCategoryString "#a -> #b -> #c"
      shouldBe result $ Right $ Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b"), Thing (Name "c")]}
    it "(Morphism) should parse named labeled morphisms" $ do
      let result = parseCategoryString "AB:#a -> #b"
      shouldBe result $ Right (Placeholder {name = Name "AB", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}})
    it "(Morphism) should need parenthesis for inner labels" $ do
      let result = parseCategoryString "(AB:#a) -> #b"
      shouldBe result $ Right (Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "AB", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}}]},Thing {name = Name "b"}]})
    -- it "(Morphism) should parse function calls" $ do
    --   let result = parseCategoryString "(c, _, _) -> sc.print (c, _)"
    --   case result of
    --     Left e -> error e
    --     Right cat -> shouldBe cat $ (Thing (Name "n"))
    it "(Tuples) should parse inner morphisms" $ do
      let result = parseCategoryString "(#a -> #b, #b -> #c)"
      shouldBe result $ Right $ Composite {composite_type = Tuple, inner_categories = [Composite {composite_type=Function, inner_categories=[Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type=Function, inner_categories=[Thing (Name "b"), Thing (Name "c")]}]}
    it "(Tuples) should handle spaces better" $ do
      let result = parseCategoryString "( #anything , #empty_list )" 
      shouldBe result $ Right $ Composite {composite_type = Tuple, inner_categories = [Thing (Name "anything"), Thing (Name "empty_list")]}
    it "(Eithers) should parse sumples" $ do
      let result = parseCategoryString "|#a, #b|" 
      shouldBe result $ Right $ Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}
    it "(Composition) should parse compositions" $ do
      let result = parseCategoryString "(#a -> #b;  #b -> #c)"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite{composite_type=Composition,inner_categories=[Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type = Function, inner_categories = [Thing (Name "b"), Thing (Name "c")]}]}
    it "(Match) should parse sumpositions" $ do
      let result = parseCategoryString "|#a -> #b; #b -> #c|"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite{composite_type=Match,inner_categories=[Composite {composite_type = Function, inner_categories = [Thing (Name "a"), Thing (Name "b")]}, Composite {composite_type = Function, inner_categories = [Thing (Name "b"), Thing (Name "c")]}]}
      let result = parseCategoryString "|(#a -> #b); (#b -> #c)|"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}
    it "(Placeholder) should parse placeholders" $ do
      let result = parseCategoryString "x@#a" 
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
      let result = parseCategoryString "{x@(#a) | #a -> #b}" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Refined {base = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "a"}]}}, predicate = Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}}
    it "(Special) should parse flexible" $ do
      let result = parseCategoryString "?test"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder (Name "test") Variable Special{special_type=Flexible}
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
      let result = parseCategoryString "tail:List list_type" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder {name = Name "tail", placeholder_kind=Label, placeholder_category = Call {base = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}
    it "(Call) should call consecutiveness" $ do
      let result = parseCategoryString "base_foo some_arg some_other" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Call{base=Call{base=Reference (Name "base_foo"),argument=Reference (Name "some_arg")},argument=Reference (Name "some_other") }
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
      let result = parseCategoryString "base_ref[name]" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name"]}
    it "(Access) should parse multiple bracket" $ do
      let result = parseCategoryString "base_ref[name, a , b]" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Access{base=Reference{name=Name "base_ref"}, access_type=ByLabelGroup [Name "name", Name "a", Name "b"]}
    it "(Recursive) should parse recursion" $ do
      let result = parseCategoryString "self:(#a -> self #b)" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Placeholder {name = Name "self", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Call {base = Reference {name = Name "self"}, argument = Thing {name = Name "b"}}]}]}}
    it "(Import) should parse import" $ do
      let result = parseCategoryString "import test.test1" 
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ Import {import_category = Access {base = Reference {name = Name "test"}, access_type =ByLabelGroup [Name "test1"]}}
    it "(TypeAnnotation) should parse type annotation" $ do
      let result = parseCategoryStringWith pPlaceholder "#a::#b"
      case result of
        Left e -> error e
        Right cat -> shouldBe cat $ TypeAnnotation {big_category = Thing {name = Name "a"}, small_category = Thing {name = Name "b"}}
    describe "File loading" $ do
      it "should load files - test1" $ do
        result <- getResultOfT $ loadTextual "Tests.test1"
        result `shouldBe` Right (Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}}]},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "a", placeholder_kind = Label, placeholder_category = Thing {name = Name "1"}},Placeholder {name = Name "b", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}}]}]}})
      it "should load files - test2" $ do
        result <- getResultOfT $ loadTextual "Tests.test2"
        result `shouldBe` Right (Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}})
    describe "Scope Parsing" $ do
      it "(Binding) should parse binds" $ do
        let result = parseCategoryStringWith pBinding "x = #something"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Binding {placeholder = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}, category_to_bind = Thing {name = Name "something"}}
      it "should parse simple scopes" $ do
        let result = parseCategoryString "{x = #something; x}"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Binding {placeholder = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}, category_to_bind = Thing {name = Name "something"}},Reference {name = Name "x"}]}
      it "should parse simple scopes with return" $ do
        let result = parseCategoryString "{x = #something; return x}"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Binding {placeholder = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}, category_to_bind = Thing {name = Name "something"}},Reference {name = Name "x"}]}
      it "should parse long scopes with non bindings" $ do
        let result = parseCategoryString "{x = #something; return x}"
        case result of
          Left err -> error err
          Right something -> something `shouldBe` Scope {statements = [Binding {placeholder = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}, category_to_bind = Thing {name = Name "something"}},Reference {name = Name "x"}]}
    describe "Example Parsing" $ do
      it "should parse Hello World" $ do
        result <- getResultOfT $ loadTextual "Examples.HelloWorld"
        case result of
          Left err -> error (show err)
          Right something -> something `shouldBe` Binding {placeholder = Placeholder {name = Name "x", placeholder_kind = Variable, placeholder_category = Special {special_type = Any}}, category_to_bind = Thing {name = Name "something"}}
