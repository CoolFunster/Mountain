{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

import qualified Test.QuickCheck as Q

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
import FrontEnds.Textual.V1.CategoryParser (parseCategoryStringWith, pCategoryOptionalLabel, pStandardLabeledCategory)

instance Q.Arbitrary SpecialCategoryType where
    arbitrary = do
        Q.oneof [
            return Flexible,
            return Reference,
            return Universal]

instance Q.Arbitrary Id where
    arbitrary = do
        arbitrary_name <- Q.vectorOf 15 (Q.choose ('a', 'z'))
        -- arbitrary_idx <- arbitrary
        Q.oneof [
            -- return Unnamed,
            return $ Name arbitrary_name]
            -- return $ Index arbitrary_idx]

instance Q.Arbitrary Category where
  arbitrary = do
        let isJustPos = (\x -> fmap (>= 0) x == Just True)
        arbitrary_name <- arbitrary :: Q.Gen Id
        Q.frequency [
            (10, Thing <$> arbitrary),
            (1, Morphism <$> arbitrary <*> arbitrary <*> arbitrary),
            (1, Placeholder <$> arbitrary <*> Q.suchThat arbitrary isJustPos <*> arbitrary),
            (3, Q.oneof [
                    return Special{name= Unnamed, special_type=Flexible},
                    return Special{name= Unnamed, special_type=Universal},
                    return Special{name=arbitrary_name, special_type=Reference}]),
            (1, MorphismCall <$> Q.suchThat arbitrary isMorphic <*> arbitrary),
            (1, Dereference <$> arbitrary <*> arbitrary),
            (2, Membership <$> arbitrary <*> arbitrary)]
            -- (1, RefinedCategory <$> arbitrary <*> arbitrary <*> arbitrary)]



spec :: Spec
spec = do
    describe "Thing Parser" $ do
        it "(Things) should parse things" $ do
            parseCategoryString "`Something" `shouldBe` Thing{name=Name "Something"}
        it "(Things) should parse parenthesized things" $ do
            parseCategoryString "(`Something)" `shouldBe` Thing{name=Name "Something"}
        it "(Tuples) should parse tuples" $ do
            parseCategoryString "(`a, `b, `c)" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Thing {name = Name "b"},Thing {name = Name "c"}]}
        it "(Tuples) should parse recursive tuples" $ do
            parseCategoryString "(`a, (`b, `c))" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "a"},Composite {name = Unnamed, composition_type = Product, inner = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}
        it "(Tuples) should parse inner morphisms" $ do
            parseCategoryString "(given `a -> return `b, given `b -> return `c)" `shouldBe` Composite {name = Unnamed, composition_type = Product, inner = [Morphism{name=Unnamed, input=Thing {name = Name "a"},output=Thing {name = Name "b"}}, Morphism{name=Unnamed, input=Thing {name = Name "b"},output=Thing {name = Name "c"}}]}
        it "(Sumples) should parse sumples" $ do
            parseCategoryString "|`a, `b|" `shouldBe` Composite {name = Unnamed, composition_type = Sum, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Higher) should parse higher categories" $ do
            parseCategoryString "^{`a,`b}" `shouldBe` Composite {name = Unnamed, composition_type = Higher, inner = [Thing {name = Name "a"},Thing {name = Name "b"}]}
        it "(Morphism) should parse morphisms" $ do
            parseCategoryString "given `a -> return `b" `shouldBe` Morphism Unnamed (Thing (Name "a")) (Thing (Name "b"))
        it "(Morphism) should parse chains" $ do
            parseCategoryString  "given `a -> given `b -> return `c" `shouldBe` Morphism Unnamed (Thing (Name "a")) (Morphism Unnamed (Thing (Name "b")) (Thing (Name "c")))
        it "(Morphism) should parse named morphisms" $ do
            parseCategoryString "AB:given `a -> return `b" `shouldBe` Morphism (Name "AB") (Thing (Name "a")) (Thing (Name "b"))
        it "(Composition) should parse compositions" $ do
            let a_to_b = parseCategoryString "given `a -> return `b"
            let b_to_c = parseCategoryString "given `b -> return `c"
            parseCategoryString "*(given `a -> return `b, given `b -> return `c)" `shouldBe` Composite{name=Unnamed,composition_type=Composition,inner=[a_to_b, b_to_c]}
        it "(Sumposition) should parse sumpositions" $ do
            let a_to_b = parseCategoryString "given `a -> return `b"
            let b_to_c = parseCategoryString "given `b -> return `c"
            parseCategoryString "*|given `a -> return `b, given `b -> return `c|" `shouldBe` Composite{name=Unnamed,composition_type=Sumposition,inner=[a_to_b, b_to_c]}
        it "(Placeholder) should parse placeholders" $ do
            parseCategoryString "x@`a" `shouldBe` Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")}
            parseCategoryString "x@(`a)" `shouldBe` Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")}
            parseCategoryString "x<1>@`a" `shouldBe` Placeholder{name=Name "x", ph_level=Just 1, ph_category=Thing (Name "a")}
            parseCategoryString "x<1>@(`a)" `shouldBe` Placeholder{name=Name "x", ph_level=Just 1, ph_category=Thing (Name "a")}
            parseCategoryString "@`a" `shouldBe` Placeholder{name=Unnamed, ph_level=Nothing, ph_category=Thing (Name "a")}
            parseCategoryString "<1>@`a" `shouldBe` Placeholder{name=Unnamed, ph_level=Just 1, ph_category=Thing (Name "a")}
        it "(Refinement) should parse refinement" $ do
            parseCategoryString "{x@(`a) | given `a -> return `b}" `shouldBe` RefinedCategory Unnamed Placeholder{name=Name "x", ph_level=Nothing, ph_category=Thing (Name "a")} Morphism{name=Unnamed, input=Thing (Name "a"), output=Thing (Name "b")}
        it "(Special) should parse flexible" $ do
            parseCategoryString "(%)" `shouldBe` Special{name=Unnamed, special_type=Flexible}
            parseCategoryString "test:(%)" `shouldBe` Special{name=Name "test", special_type=Flexible}
        it "(Special) should parse Universal" $ do
            parseCategoryString "Any" `shouldBe` Special{name=Unnamed, special_type=Universal}
            parseCategoryString "test:Any" `shouldBe` Special{name=Name "test", special_type=Universal}
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
        it "(Recursive) should parse recursion" $ do
            parseCategoryString "self:(given `a -> return $self[`b])" `shouldBe` Morphism {name = Name "self", input = Thing {name = Name "a"}, output = MorphismCall {base_morphism = Special {name = Name "self", special_type = Reference}, argument = Thing {name = Name "b"}}}
    -- describe "read" $ do
    --     it "Parser Writer property check" $ Q.property $
    --         \x -> (parseCategoryString . categoryToText) x `shouldBe` (x :: Category)
    --     it "should handle weird inputs" $ do
    --         -- let result = Placeholder {name = Name "snjtohdxzhzduha", ph_level = Just 0, ph_category = Morphism {name = Name "yqwjtynfczbpylb", input = Thing {name = Name "teqtbwxzcgzbzzs"}, output = Morphism {name = Name "lrwtclekizeecdu", input = Special {name = Unnamed, special_type = Flexible}, output = Dereference {base_category = Thing {name = Name "qaxdhkrfcwrdazf"}, category_id = Name "pbyuagseuymmttb"}}}}
    --         let result = Morphism {name = Name "yqwjtynfczbpylb", input = Thing {name = Name "teqtbwxzcgzbzzs"}, output = Morphism {name = Name "lrwtclekizeecdu", input = Special {name = Unnamed, special_type = Flexible}, output = Dereference {base_category = Thing {name = Name "qaxdhkrfcwrdazf"}, category_id = Name "pbyuagseuymmttb"}}}
    --         -- {snjtohdxzhzduha<0>@(yqwjtynfczbpylb:`teqtbwxzcgzbzzs -> lrwtclekizeecdu:swppevicyohjvch:(%) -> `qaxdhkrfcwrdazf.pbyuagseuymmttb)
    --         let new_result = categoryToText result
    --         print result
    --         TextIO.putStrLn new_result
    --         let parsed_result = parseCategoryString $ new_result
    --         print parsed_result
    --         parsed_result `shouldBe` result
    --     it "Parser Writer property check" $ Q.property $
    --         \x -> (parseCategoryString . categoryToText) x `shouldBe` (x :: Category)
    --         result <- Q.generate $ (arbitrary :: Q.Gen Category)
    --         let new_result = categoryToText result
    --         putStrLn $ show result
    --         TextIO.putStrLn new_result
    --         putStrLn $ show $ parseCategoryString $ new_result
            