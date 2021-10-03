{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.CategoryParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import CategoryData
import FrontEnds.Textual.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import FrontEnds.Textual.CategoryParser
import FrontEnds.Textual.CategoryParser (pPlaceholder)

-- TODO Split into respective files
parseCategory = parse pCategory ""

spec :: Spec
spec = do
    describe "Thing Parser" $ do
        it "(Things) should parse things" $ do
            parseCategory "`Something" `shouldParse` Thing{name=Name "Something"}
        it "(Tuples) should parse tuples" $ do
            parseCategory "(`a, `b, `c)" `shouldParse` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
        it "(Tuples) should parse recursive tuples" $ do
            parseCategory "(`a, (`b, `c))" `shouldParse` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
        it "(Sumples) should parse sumples" $ do
            parseCategory "|`a, `b|" `shouldParse` Composite {name = Unnamed, composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Higher) should parse higher categories" $ do
            parseCategory "^{`a,`b}" `shouldParse` Composite {name = Unnamed, composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Morphism) should parse morphisms" $ do
            parseCategory "`a -> `b" `shouldParse` Morphism Unnamed (Thing (Name "a")) (Thing (Name "b"))
        it "(Morphism) should parse named morphisms" $ do
            parseCategory "AB:`a -> `b" `shouldParse` Morphism (Name "AB") (Thing (Name "a")) (Thing (Name "b"))
        it "(Tuples) should parse inner morphisms" $ do
            parseCategory "(`a -> `b)" `shouldParse` Composite {name = Unnamed, composition_type = Product, inner = [Morphism{name=Unnamed, input=Thing {name = Name "a"},output=Thing {name = Name "b"}}]}
        it "(Composition) should parse compositions" $ do
            let a_to_b = fromRight (error "something") (parseCategory "`a -> `b")
            let b_to_c = fromRight (error "something") (parseCategory "`b -> `c")
            parseCategory "*(`a -> `b, `b -> `c)" `shouldParse` Composite{name=Unnamed,composition_type=Composition,inner=[a_to_b, b_to_c]}
        it "(Sumposition) should parse sumpositions" $ do
            let a_to_b = fromRight (error "something") (parseCategory "`a -> `b")
            let b_to_c = fromRight (error "something") (parseCategory "`b -> `c")
            parseCategory "*|`a -> `b, `b -> `c|" `shouldParse` Composite{name=Unnamed,composition_type=Sumposition,inner=[a_to_b, b_to_c]}
        it "(Placeholder) should parse placeholders" $ do
            parse pCategory "" "x@`a" `shouldParse` Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")}