{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.Categories.ListSpec (spec) where

import Test.Hspec

import CategoryData
import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter

import Data.Either

import Text.Megaparsec.Debug (dbg)
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryPrintableChar)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import System.Posix.Internals (puts)
import qualified Data.Text.IO as TextIO
import Debug.Trace
import Data.List (intercalate)

spec :: Spec
spec = do
    describe "Category List" $ do
        let executeTextual = fmap fst . runCategoryContextT . execute (Options{reduce_composite=False, importer=loadTextual})
        let executeToCategory = \c -> do {
            result <- executeTextual c;
            return $ resolveReferences $ fromRight (error "404") result;
        }
        describe "list" $ do
            let has' a b = getResultOf $ has a b
            it "(list) should substitute type correctly on evaluate" $ do
                execute_result <- executeToCategory (parseCategoryString "(import $base.linkedlist)[`1].list")
                categoryToString execute_result `shouldBe` "list_def:|`empty,nonempty:(`1,$list_def)|"
            it "(list) should have the empty" $ do
                let empty_test = parseCategoryString "`empty"
                execute_result <- executeToCategory (parseCategoryString "(import $base.linkedlist)[`1].list")
                execute_result `has'` empty_test `shouldBe` Right True
            it "(list) should have the single elem list" $ do
                let test1 = parseCategoryString "(`1,`empty)"
                execute_result <- executeToCategory (parseCategoryString "(import $base.linkedlist)[`1].list")
                execute_result `has'` test1 `shouldBe` Right True
            it "(list) should have the two elem list" $ do
                let test2 = parseCategoryString "(`1,(`1,`empty))"
                execute_result <- executeToCategory (parseCategoryString "(import $base.linkedlist)[`1].list")
                execute_result `has'` test2 `shouldBe` Right True
            it "(list) should have the three elem list" $ do
                let test3 = parseCategoryString "(`1,(`1,(`1,`empty)))"
                execute_result <- executeToCategory (parseCategoryString "(import $base.linkedlist)[`1].list")
                execute_result `has'` test3 `shouldBe` Right True
        describe "join" $ do
            it "(join) should properly access join" $ do
                let list_on_1_join = parseCategoryString "(import $base.linkedlist)[`1].join"
                -- dbg_result <- executeTextual list_on_1_join
                -- putStrLn (intercalate "\n====\n" $ map tracedToString dbg_result)
                execute_result <- executeToCategory list_on_1_join
                categoryToString execute_result `shouldBe` "new_head@`1->original_list@list_def:|`empty,nonempty:(<`1>,$list_def)|->(list_def:|`empty,nonempty:(<`1>,$list_def)|)::(($new_head,$original_list))"
            it "(join) should properly join on empty" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`1][`empty]"
                execute_result <- executeToCategory list_on_1_join_empty
                categoryToString execute_result `shouldBe` "(`1,`empty)"
            it "(join) should properly join on length 1 list" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`1][(`1, `empty)]"
                execute_result <- executeToCategory list_on_1_join_empty
                categoryToString execute_result `shouldBe` "(`1,(`1,`empty))"
            it "(join) should reject empty head" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`empty][(`1, `empty)]"
                execute_result <- executeTextual list_on_1_join_empty
                isLeft execute_result `shouldBe` True
                error_type (head $ fromLeft (error "404") execute_result) `shouldBe` InvalidArgument
            it "should reject empty head" $ do
                let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`empty][(`1, `empty)]"
                execute_result <- executeTextual list_on_1_join_empty
                isLeft execute_result `shouldBe` True
                error_type (head $ fromLeft (error "404") execute_result) `shouldBe` InvalidArgument
        describe "head" $ do
            it "should be accessed properly" $ do
                let list_on_1_head = parseCategoryString "(import $base.linkedlist)[`1].head"
                execute_result <- executeToCategory list_on_1_head
                categoryToString execute_result `shouldBe` "input@((list_def:|`empty,nonempty:(<`1>,$list_def)|).nonempty)->(`1)::(($input).#0)"
            it "should be returning the head elem 1" $ do
                let test_case = parseCategoryString "(import $base.linkedlist)[`1].head[(`1, `empty)]"
                execute_result <- executeToCategory test_case
                categoryToString execute_result `shouldBe` "`1"
            it "should be returning the head elem 2" $ do
                let test_case = parseCategoryString "(import $base.linkedlist)[|`1, `2|].head[(`2, (`1, `empty))]"
                -- somethings not getting evaluated correctly
                execute_result <- executeToCategory test_case
                categoryToString execute_result `shouldBe` "`2"
        describe "tail" $ do
            it "should be accessed properly" $ do
                let test_case = parseCategoryString "(import $base.linkedlist)[`1].tail"
                execute_result <- executeToCategory test_case
                categoryToString execute_result `shouldBe` "input@((list_def:|`empty,nonempty:(<`1>,$list_def)|).nonempty)->(list_def:|`empty,nonempty:(<`1>,$list_def)|)::(($input).#1)"
            it "should be returning the tail elem 1" $ do
                let test_case = parseCategoryString "(import $base.linkedlist)[`1].tail[(`1, `empty)]"
                -- somethings not getting evaluated correctly
                execute_result <- executeToCategory test_case
                -- putStrLn $ errorableToString execute_result
                categoryToString execute_result `shouldBe` "`empty"
            it "should be returning the tail elem 2" $ do
                let test_case = parseCategoryString "(import $base.linkedlist)[|`1, `2|].tail[(`2, (`1, `empty))]"
                execute_result <- executeToCategory test_case
                -- putStrLn $ errorableToString execute_result
                categoryToString execute_result `shouldBe` "(`1,`empty)"
        -- describe "for" $ do
        --     it "should be accessed properly" $ do
        --         let test_case = parseCategoryString "(import $base.linkedlist)[`1].for"
        --         execute_result <- executeToCategory test_case
        --         -- isRight execute_result `shouldBe` True
        --         categoryToString (execute_result) `shouldBe` "input@((list_def:|`empty,nonempty:(`1,$list_def)|).nonempty)->(list_def:|`empty,nonempty:(`1,$list_def)|)::(($input).#1)"
        --     it "should be folding correctly" $ do
        --         let test_case = parseCategoryString "((import $base.linkedlist)[`1].for)[|`1,`2|][`1][(`1, `empty)][(given `1 -> given prev_state@|`1,`2| -> return `2)]"
        --         execute_result <- executeTextual test_case
        --         putStrLn $ errorableToString execute_result
                -- isRight execute_result `shouldBe` True
                -- categoryToString (execute_result) `shouldBe` "(`1,`empty)"