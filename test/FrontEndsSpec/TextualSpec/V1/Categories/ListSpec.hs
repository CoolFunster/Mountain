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
        describe "list" $ do
            it "(list) should substitute type correctly on evaluate" $ do
                execute_result <- runErrorableT $ executeTextual (parseCategoryString "(import $base.list)[`1].list")
                categoryToString (fromValid execute_result) `shouldBe` "list_def:|`empty,nonempty:(`1,$list_def)|"
            it "(list) should have the empty" $ do
                let empty_test = parseCategoryString "`empty"
                execute_result <- runErrorableT $ executeTextual (parseCategoryString "(import $base.list)[`1].list")
                fromValid execute_result `tracedHas` empty_test `shouldBe` Valid True
            it "(list) should have the single elem list" $ do
                let test1 = parseCategoryString "(`1,`empty)"
                execute_result <- runErrorableT $ executeTextual (parseCategoryString "(import $base.list)[`1].list")
                fromValid execute_result `tracedHas` test1 `shouldBe` Valid True
            it "(list) should have the two elem list" $ do
                let test2 = parseCategoryString "(`1,(`1,`empty))"
                execute_result <- runErrorableT $ executeTextual (parseCategoryString "(import $base.list)[`1].list")
                fromValid execute_result `tracedHas` test2 `shouldBe` Valid True
            it "(list) should have the three elem list" $ do
                let test3 = parseCategoryString "(`1,(`1,(`1,`empty)))"
                execute_result <- runErrorableT $ executeTextual (parseCategoryString "(import $base.list)[`1].list")
                fromValid execute_result `tracedHas` test3 `shouldBe` Valid True
        describe "join" $ do
            it "(join) should properly access join" $ do
                let list_on_1_join = parseCategoryString "(import $base.list)[`1].join"
                execute_result <- runErrorableT $ executeTextual list_on_1_join
                categoryToString (fromValid execute_result) `shouldBe` "new_head@`1->original_list@list_def:|`empty,nonempty:(`1,$list_def)|->(list_def:|`empty,nonempty:(`1,$list_def)|)::(($new_head,$original_list))"
            it "(join) should properly join on empty" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.list)[`1].join)[`1][`empty]"
                execute_result <- runErrorableT $ executeTextual list_on_1_join_empty
                categoryToString (fromValid execute_result) `shouldBe` "(`1,`empty)"
            it "(join) should properly join on length 1 list" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.list)[`1].join)[`1][(`1, `empty)]"
                execute_result <- runErrorableT $ executeTextual list_on_1_join_empty
                categoryToString (fromValid execute_result) `shouldBe` "(`1,(`1,`empty))"
            it "(join) should reject empty head" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.list)[`1].join)[`empty][(`1, `empty)]"
                execute_result <- runErrorableT $ executeTextual list_on_1_join_empty
                isError execute_result `shouldBe` True
                error_type (head $ errors execute_result) `shouldBe` InvalidArgument
            it "should reject empty head" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.list)[`1].join)[`empty][(`1, `empty)]"
                execute_result <- runErrorableT $ executeTextual list_on_1_join_empty
                isError execute_result `shouldBe` True
                error_type (head $ errors execute_result) `shouldBe` InvalidArgument
        describe "head" $ do
            it "should be accessed properly" $ do
                let list_on_1_head = parseCategoryString "(import $base.list)[`1].head"
                execute_result <- runErrorableT $ executeTextual list_on_1_head
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "input@((list_def:|`empty,nonempty:(`1,$list_def)|).nonempty)->(`1)::(($input).#0)"
            it "should be returning the head elem 1" $ do
                let test_case = parseCategoryString "(import $base.list)[`1].head[(`1, `empty)]"
                -- somethings not getting evaluated correctly
                execute_result <- runErrorableT $ executeTextual test_case
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "`1"
            it "should be returning the head elem 2" $ do
                let test_case = parseCategoryString "(import $base.list)[|`1, `2|].head[(`2, (`1, `empty))]"
                -- somethings not getting evaluated correctly
                execute_result <- runErrorableT $ executeTextual test_case
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "`2"
        describe "tail" $ do
            it "should be accessed properly" $ do
                let test_case = parseCategoryString "(import $base.list)[`1].tail"
                execute_result <- runErrorableT $ executeTextual test_case
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "input@((list_def:|`empty,nonempty:(`1,$list_def)|).nonempty)->(list_def:|`empty,nonempty:(`1,$list_def)|)::(($input).#1)"
            it "should be returning the head elem 1" $ do
                let test_case = parseCategoryString "(import $base.list)[`1].tail[(`1, `empty)]"
                -- somethings not getting evaluated correctly
                execute_result <- runErrorableT $ executeTextual test_case
                putStrLn $ errorableToString execute_result
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "`empty"
            it "should be returning the head elem 2" $ do
                let test_case = parseCategoryString "(import $base.list)[|`1, `2|].tail[(`2, (`1, `empty))]"
                -- somethings not getting evaluated correctly
                execute_result <- runErrorableT $ executeTextual test_case
                putStrLn $ errorableToString execute_result
                isValid execute_result `shouldBe` True
                categoryToString (fromValid execute_result) `shouldBe` "(`1,`empty)"