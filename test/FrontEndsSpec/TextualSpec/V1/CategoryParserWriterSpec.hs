{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserWriterSpec (spec) where

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
    describe "Writer must be parseable by parser" $ do
        -- it "Parser Writer property check" $ Q.property $
        --     \x -> (parseCategoryString . categoryToText) x `shouldBe` (x :: Category)
        it "should handle weird inputs" $ do
            -- let result = Placeholder {name = Name "snjtohdxzhzduha", ph_level = Just 0, ph_category = Morphism {name = Name "yqwjtynfczbpylb", input = Thing {name = Name "teqtbwxzcgzbzzs"}, output = Morphism {name = Name "lrwtclekizeecdu", input = Special {name = Unnamed, special_type = Flexible}, output = Dereference {base_category = Thing {name = Name "qaxdhkrfcwrdazf"}, category_id = Name "pbyuagseuymmttb"}}}}
            let result = Morphism {name = Name "yqwjtynfczbpylb", input = Thing {name = Name "teqtbwxzcgzbzzs"}, output = Morphism {name = Name "lrwtclekizeecdu", input = Special {name = Unnamed, special_type = Flexible}, output = Dereference {base_category = Thing {name = Name "qaxdhkrfcwrdazf"}, category_id = Name "pbyuagseuymmttb"}}}
            -- {snjtohdxzhzduha<0>@(yqwjtynfczbpylb:`teqtbwxzcgzbzzs -> lrwtclekizeecdu:swppevicyohjvch:(%) -> `qaxdhkrfcwrdazf.pbyuagseuymmttb)
            let new_result = categoryToText result
            print result
            TextIO.putStrLn new_result
            let parsed_result = parseCategoryString $ new_result
            print parsed_result
            parsed_result `shouldBe` result
        -- it "Parser Writer property check" $ Q.property $
        --     \x -> (parseCategoryString . categoryToText) x `shouldBe` (x :: Category)
        --     result <- Q.generate $ (arbitrary :: Q.Gen Category)
        --     let new_result = categoryToText result
        --     putStrLn $ show result
        --     TextIO.putStrLn new_result
        --     putStrLn $ show $ parseCategoryString $ new_result
            