{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.Categories.Base.Data.ListSpec (spec) where

import Test.Hspec

import Category
import FrontEnds.Textual.V1.Mountain
import FrontEnds.Textual.V1.CategoryWriter

spec :: Spec
spec = do
    describe "Category List" $ do
      describe "list" $ do
        let has' a b = getResultOf $ has a b
        it "(list) should substitute type correctly on evaluate" $ do
            result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.LinkedList) {#1}"
            putStrLn $ prettyCategoryToString result
            shouldBe result $ Composite {composite_type = Either, inner_categories = [Placeholder {name = Name "empty", placeholder_kind = Label, placeholder_category = Set {elements = [Thing {name = Name "empty"}]}},Placeholder {name = Name "nonempty", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Call {base = Placeholder {name = Name "LinkedList", placeholder_kind = Resolved, placeholder_category = Placeholder {name = Name "LinkedList", placeholder_kind = Label, placeholder_category = Composite {composite_type = Function, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "ListType", placeholder_kind = Label, placeholder_category = Special {special_type = Flexible}}]},Composite {composite_type = Either, inner_categories = [Placeholder {name = Name "empty", placeholder_kind = Label, placeholder_category = Set {elements = [Thing {name = Name "empty"}]}},Placeholder {name = Name "nonempty", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Call {base = Reference {name = Name "LinkedList"}, argument = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}}}]}}]}]}}}, argument = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}}}]}}]}
        it "(list) should have the empty" $ do
            empty_test <- runMountainString strict "#empty"
            result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.LinkedList) {#1}"
            (result `has'` empty_test) `shouldBe` Right True
        it "(list) should have the single elem list" $ do
            test1 <- runMountainString strict  "(#1,#empty)"
            execute_result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.LinkedList) {#1}"
            let has_res = execute_result `has` test1
            -- putStrLn $ categoryLogsToString $ getLogOf has_res
            getResultOf has_res `shouldBe` Right True
        it "(list) should have the two elem list" $ do
            test1 <- runMountainString strict  "(#1,(#1,#empty))"
            execute_result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.LinkedList) {#1}"
            let has_res = execute_result `has` test1
            -- putStrLn $ categoryLogsToString $ getLogOf has_res
            getResultOf has_res `shouldBe` Right True
        it "(list) should have the three elem list" $ do
            test1 <- runMountainString strict  "(#1,(#1,(#1,#empty)))"
            execute_result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.LinkedList) {#1}"
            let has_res = execute_result `has` test1
            -- putStrLn $ categoryLogsToString $ getLogOf has_res
            getResultOf has_res `shouldBe` Right True
    -- describe "push" $ do
    --     it "(push) should properly access push" $ do
    --       result <- runMountainString strict "import (l:Base.Data.Basic.linkedList) -> (l.push)"
    --       shouldBe result $ Placeholder {name = Name "LinkedList", placeholder_kind = Label, placeholder_category = Composite {composite_type = Either, inner_categories = [Placeholder {name = Name "empty", placeholder_kind = Label, placeholder_category = Set {elements = [Thing {name = Name "empty"}]}},Placeholder {name = Name "nonempty", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "head", placeholder_kind = Label, placeholder_category = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}},Placeholder {name = Name "tail", placeholder_kind = Label, placeholder_category = Call {base = Reference {name = Name "LinkedList"}, argument = Placeholder {name = Name "ListType", placeholder_kind = Resolved, placeholder_category = Set {elements = [Thing {name = Name "1"}]}}}}]}}]}}
    --             let list_on_1_join = parseCategoryString "(import $base.linkedlist)[`1].join"
    --             -- dbg_result <- executeTextual list_on_1_join
    --             -- putStrLn (intercalate "\n====\n" $ map tracedToString dbg_result)
    --             let ctx_result = executePlain list_on_1_join
    --             execute_result <- getResultOfT ctx_result
    --             log_result <- getLogOfT ctx_result
    --             categoryToString (fromRight' execute_result) `shouldBe` "new_head@<`1>->original_list@<list_def:|empty:{`empty},nonempty:(<`1>,$list_def)|>->(<list_def:|empty:{`empty},nonempty:(<`1>,$list_def)|>)::(($new_head,$original_list))"
    --         it "(join) should properly join on empty" $ do
    --             let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`1][`empty]"
    --             execute_result <- executeToCategory list_on_1_join_empty
    --             categoryToString execute_result `shouldBe` "(`1,`empty)"
    --         it "(join) should properly join on length 1 list" $ do
    --             let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`1][(`1, `empty)]"
    --             execute_result <- executeToCategory list_on_1_join_empty
    --             categoryToString execute_result `shouldBe` "(`1,(`1,`empty))"
    --         it "(join) should reject empty head" $ do
    --             let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`empty][(`1, `empty)]"
    --             execute_result <- executeTextual list_on_1_join_empty
    --             isLeft execute_result `shouldBe` True
    --             error_type (head $ fromLeft (error "404") execute_result) `shouldBe` InvalidArgument
    --         it "should reject empty head" $ do
    --             let list_on_1_join_empty = parseCategoryString "((import $base.linkedlist)[`1].join)[`empty][(`1, `empty)]"
    --             execute_result <- executeTextual list_on_1_join_empty
    --             isLeft execute_result `shouldBe` True
    --             error_type (head $ fromLeft (error "404") execute_result) `shouldBe` InvalidArgument
    --     describe "head" $ do
    --         it "should be accessed properly" $ do
    --             let list_on_1_head = parseCategoryString "(import $base.linkedlist)[`1].head"
    --             execute_result <- executeToCategory list_on_1_head
    --             categoryToString execute_result `shouldBe` "input@((list_def:|`empty,nonempty:(<`1>,$list_def)|).nonempty)->(`1)::(($input).#0)"
    --         it "should be returning the head elem 1" $ do
    --             let test_case = parseCategoryString "(import $base.linkedlist)[`1].head[(`1, `empty)]"
    --             execute_result <- executeToCategory test_case
    --             categoryToString execute_result `shouldBe` "`1"
    --         it "should be returning the head elem 2" $ do
    --             let test_case = parseCategoryString "(import $base.linkedlist)[|`1, `2|].head[(`2, (`1, `empty))]"
    --             -- somethings not getting evaluated correctly
    --             execute_result <- executeToCategory test_case
    --             categoryToString execute_result `shouldBe` "`2"
    --     describe "tail" $ do
    --         it "should be accessed properly" $ do
    --             let test_case = parseCategoryString "(import $base.linkedlist)[`1].tail"
    --             execute_result <- executeToCategory test_case
    --             categoryToString execute_result `shouldBe` "input@((list_def:|empty:{`empty},nonempty:(<`1>,$list_def)|).nonempty)->(list_def:|empty:{`empty},nonempty:(<`1>,$list_def)|)::(($input).#1)"
    --         it "should be returning the tail elem 1" $ do
    --             let test_case = parseCategoryString "(import $base.linkedlist)[`1].tail[(`1, `empty)]"
    --             -- somethings not getting evaluated correctly
    --             execute_result <- executeToCategory test_case
    --             -- putStrLn $ errorableToString execute_result
    --             categoryToString execute_result `shouldBe` "`empty"
    --         it "should be returning the tail elem 2" $ do
    --             let test_case = parseCategoryString "(import $base.linkedlist)[|`1, `2|].tail[(`2, (`1, `empty))]"
    --             execute_result <- executeToCategory test_case
    --             -- putStrLn $ errorableToString execute_result
    --             categoryToString execute_result `shouldBe` "(`1,`empty)"
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