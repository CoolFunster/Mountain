{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.Categories.ListSpec (spec) where

import Test.Hspec

import CategoryData
import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter

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
    describe "Category List" $ do
        let executeTextual = execute loadTextual
        let parsedCategory = runErrorableT (executeTextual (parseCategoryString "import $base.list"))   
        it "should parse correctly" $ do
            result <- parsedCategory
            categoryToString (fromValid result) `shouldBe` "list_type@Any->define list:|`empty,nonempty:($list_type,$list)|->$list"
        let empty_test = parseCategoryString "`empty"
        it "should substitute type correctly on evaluate" $ do
            result <- parsedCategory
            execute_result <- runErrorableT $ stepEvaluate loadTextual (FunctionCall (fromValid result) (Thing (Name "1")))
            categoryToString (fromValid execute_result) `shouldBe` "define list:|`empty,nonempty:(`1,$list)|->$list"
        it "should have the empty" $ do
            result <- parsedCategory
            let parsed_list = fromValid result
            let result = FunctionCall parsed_list (Thing (Name "1")) `tracedHas` empty_test
            result `shouldBe` Valid True
        let test1 = parseCategoryString "(`1,`empty)"
        it "should have the single elem list" $ do
            result <- parsedCategory
            let parsed_list = fromValid result
            let result = FunctionCall parsed_list (Thing (Name "1")) `tracedHas` test1
            result `shouldBe` Valid True
        let test2 = parseCategoryString "(`1,(`1,`empty))"
        it "should have the two elem list" $ do
            result <- parsedCategory
            let parsed_list = fromValid result
            let result = FunctionCall parsed_list (Thing (Name "1")) `tracedHas` test2
            result `shouldBe` Valid True
        let test3 = parseCategoryString "(`1,(`1,(`1,`empty)))"
        it "should have the three elem list" $ do
            result <- parsedCategory
            let parsed_list = fromValid result
            let result = FunctionCall parsed_list (Thing (Name "1")) `tracedHas` test3
            result `shouldBe` Valid True    