{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.Categories.NatSpec (spec) where

import Test.Hspec

import CategoryData
import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter

import Data.Either

import Debug.Trace
import Data.List (intercalate)


spec :: Spec
spec = do
    describe "Category Nat" $ do
        let executeTextual = \c -> fmap fst $ runCategoryContextT $ execute (Options{reduce_composite=True, importer=loadTextual}) c
        let executeLog = \c -> fmap snd $ runCategoryContextT $ execute (Options{reduce_composite=True, importer=loadTextual}) c
        it "(module) should be valid" $ do
            parsedCategory <- executeTextual (parseCategoryString "import $base.natural")
            isRight parsedCategory `shouldBe` True
            -- putStrLn $ prettyCategoryToString (fromValid parsedCategory)
        describe "has" $ do
            let has' a b = fst $ runCategoryContext $ has a b
            it "should have zero" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).natural")
                zero <- executeTextual (parseCategoryString "`zero")
                isRight parsedCategory `shouldBe` True
                isRight zero `shouldBe` True
                fromRight (error "404") parsedCategory `has'` fromRight (error "404") zero `shouldBe` Right True
            it "should have one" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).natural")
                one <- executeTextual (parseCategoryString "(`successor, `zero)")
                isRight parsedCategory `shouldBe` True
                isRight one `shouldBe` True
                fromRight (error "404") parsedCategory `has'` fromRight (error "404") one `shouldBe` return True
        describe "increment" $ do
            it "should increment zero" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).increment[`zero]")
                isRight parsedCategory `shouldBe` True
                categoryToString (replaceResolved $ fromRight (error "404") parsedCategory) `shouldBe` "(`successor,`zero)"
            it "should increment one" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).increment[(`successor, `zero)]")
                -- putStrLn (errorableToString parsedCategory)
                isRight parsedCategory `shouldBe` True
                categoryToString (replaceResolved $ fromRight (error "404") parsedCategory) `shouldBe` "(`successor,(`successor,`zero))"
        describe "decrement" $ do
            it "should decrement one" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).decrement[(`successor, `zero)]")
                -- putStrLn (errorableToString parsedCategory)
                isRight parsedCategory `shouldBe` True
                categoryToString (fromRight (error "404") parsedCategory) `shouldBe` "`zero"
            it "should err on zero" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).decrement[`zero]")
                -- putStrLn (errorableToString parsedCategory)
                isLeft parsedCategory `shouldBe` True
                error_type (head $ fromLeft (error "404") parsedCategory) `shouldBe` InvalidArgument
        describe "add" $ do
            it "should add zero and zero" $ do
                parsedCategory <- executeTextual (parseCategoryString "(import $base.natural).add[(`zero, `zero)]")
                -- putStrLn (errorableToString parsedCategory)
                isRight parsedCategory `shouldBe` True
                -- putStrLn (intercalate "\n\n" $ map categoryToString $ snd $ fromRight (error "404") parsedCategory)
                categoryToString (fromRight (error "404") parsedCategory) `shouldBe` "`zero"
            it "should add one and zero" $ do
                result <- executeTextual (parseCategoryString "(import $base.natural).add[((`successor, `zero),`zero)]")
                -- let result = output $ last parsedCategory
                isRight result `shouldBe` True

                -- s1 <- dbgEvaluate True loadTextual (fromRight (error "404") result)
                categoryToString (replaceResolved $ fromRight (error "404") result) `shouldBe` "(`successor,`zero)"
            it "should add one and one" $ do
                result <- executeTextual (parseCategoryString "(import $base.natural).add[((`successor, `zero),(`successor, `zero))]")
                -- putStrLn $ intercalate "\n" $ map (categoryLogToString) result
                -- let result = output $ last parsedCategory
                isRight result `shouldBe` True
                -- s1 <- multipleStepEvaluate 10 True loadTextual (fromRight (error "404") result)
                categoryToString (replaceResolved $ fromRight (error "404") result) `shouldBe` "(`successor,(`successor,`zero))"
                