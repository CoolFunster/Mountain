{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.CategoryParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import CategoryData
import FrontEnds.Textual.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import FrontEnds.Textual.CategoryParser (parseCategoryString)
import Text.Megaparsec.Debug (dbg)

-- TODO Split into respective files

spec :: Spec
spec = do
    describe "Thing Parser" $ do
        it "(Things) should parse things" $ do
            parseCategoryString "`Something" `shouldBe` Thing{name=Name "Something"}
        it "(Tuples) should parse tuples" $ do
            parseCategoryString "(`a, `b, `c)" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
        it "(Tuples) should parse recursive tuples" $ do
            parseCategoryString "(`a, (`b, `c))" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
        it "(Sumples) should parse sumples" $ do
            parseCategoryString "|`a, `b|" `shouldBe` Composite {name = Unnamed, composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Higher) should parse higher categories" $ do
            parseCategoryString "^{`a,`b}" `shouldBe` Composite {name = Unnamed, composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Morphism) should parse morphisms" $ do
            parseCategoryString "`a -> `b" `shouldBe` Morphism Unnamed (Thing (Name "a")) (Thing (Name "b"))
        it "(Morphism) should parse chains" $ do    
            parseCategoryString "`a -> `b -> `c" `shouldBe` Morphism Unnamed (Thing (Name "a")) (Morphism Unnamed (Thing (Name "b")) (Thing (Name "c")))
        it "(Morphism) should parse named morphisms" $ do
            parseCategoryString "AB:`a -> `b" `shouldBe` Morphism (Name "AB") (Thing (Name "a")) (Thing (Name "b"))
        it "(Tuples) should parse inner morphisms" $ do
            parseCategoryString "(`a -> `b, `b -> `c)" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Morphism{name=Unnamed, input=Thing {name = Name "a"},output=Thing {name = Name "b"}}, Morphism{name=Unnamed, input=Thing {name = Name "b"},output=Thing {name = Name "c"}}]}
        it "(Composition) should parse compositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*(`a -> `b, `b -> `c)" `shouldBe` Composite{name=Unnamed,composition_type=Composition,inner=[a_to_b, b_to_c]}
        it "(Sumposition) should parse sumpositions" $ do
            let a_to_b = parseCategoryString "`a -> `b"
            let b_to_c = parseCategoryString "`b -> `c"
            parseCategoryString "*|`a -> `b, `b -> `c|" `shouldBe` Composite{name=Unnamed,composition_type=Sumposition,inner=[a_to_b, b_to_c]}
        it "(Placeholder) should parse placeholders" $ do
            parseCategoryString "{x@(`a)}" `shouldBe` Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")}
            parseCategoryString "{x<1>@(`a)}" `shouldBe` Placeholder{name=Name "x", ph_level=Just 1, ph_category=Thing (Name "a")}
        it "(Refinement) should parse refinement" $ do
            parseCategoryString "{x@(`a) | `a -> `b}" `shouldBe` RefinedCategory Unnamed Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")} Morphism{name=Unnamed, input=Thing (Name "a"), output=Thing (Name "b")}
        it "(Special) should parse flexible" $ do
            parseCategoryString "(%)" `shouldBe` Special{name=Unnamed, special_type=Flexible}
            parseCategoryString "test:(%)" `shouldBe` Special{name=Name "test", special_type=Flexible}
        it "(Special) should parse Universal" $ do
            parseCategoryString "%Any" `shouldBe` Special{name=Unnamed, special_type=Universal}
            parseCategoryString "test:%Any" `shouldBe` Special{name=Name "test", special_type=Universal}
        it "(Special) should parse Reference" $ do
            parseCategoryString "$Stuff" `shouldBe` Special (Name "Stuff") Reference
        it "(Call) should call" $ do
            parseCategoryString "$base_foo[$some_arg]" `shouldBe` MorphismCall{base_morphism=Special (Name "base_foo") Reference,argument=Special (Name "some_arg") Reference}
        it "(Call) should call consecutiveness" $ do
            parseCategoryString "$base_foo[$some_arg][$some_other]" `shouldBe` MorphismCall{base_morphism=MorphismCall{base_morphism=Special (Name "base_foo") Reference,argument=Special (Name "some_arg") Reference},argument=Special (Name "some_other") Reference}
        it "(Dereference) should parse dereferences" $ do
            parseCategoryString "$base_ref.name" `shouldBe` Dereference{base_category=Special{name=Name "base_ref", special_type=Reference}, category_id=Name "name"} 
            parseCategoryString "$base_ref.name.name" `shouldBe` Dereference{base_category=Dereference{base_category=Special{name=Name "base_ref", special_type=Reference}, category_id=Name "name"}, category_id=Name "name"}
        it "(Membership) should parse membership" $ do
            parseCategoryString "$base_category::$child_category" `shouldBe` Membership{big_category=Special (Name "base_category") Reference, small_category=Special (Name "child_category") Reference}
        -- it "(Recursive) should parse recursion" $ do
        --     print $ parseCategoryString "self:(`a -> $self[`b])"