{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.Categories.NatSpec (spec) where

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
import Data.List (intercalate)


spec :: Spec
spec = do
    describe "Category Nat" $ do
        let executeTextual = execute False loadTextual
        let dbgExecuteTextual = dbgExecute False loadTextual
        it "(module) should be valid" $ do
            parsedCategory <- runErrorableT (executeTextual (parseCategoryString "import $base.natural"))
            isValid parsedCategory `shouldBe` True
            putStrLn $ prettyCategoryToString (fromValid parsedCategory)
        describe "has" $ do
            it "should have zero" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).natural"))
                zero <- runErrorableT (executeTextual (parseCategoryString "`zero"))
                isValid parsedCategory `shouldBe` True
                isValid zero `shouldBe` True
                fromValid parsedCategory `has` fromValid zero `shouldBe` return True
            it "should have one" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).natural"))
                one <- runErrorableT (executeTextual (parseCategoryString "(`successor, `zero)"))
                isValid parsedCategory `shouldBe` True
                isValid one `shouldBe` True
                fromValid parsedCategory `has` fromValid one `shouldBe` return True
        describe "increment" $ do
            it "should increment zero" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).increment[`zero]"))
                isValid parsedCategory `shouldBe` True
                categoryToString (replaceResolved $ fromValid parsedCategory) `shouldBe` "(`successor,`zero)"
            it "should increment one" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).increment[(`successor, `zero)]"))
                -- putStrLn (errorableToString parsedCategory)
                isValid parsedCategory `shouldBe` True
                categoryToString (replaceResolved $ fromValid parsedCategory) `shouldBe` "(`successor,(`successor,`zero))"
        describe "decrement" $ do
            it "should decrement one" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).decrement[(`successor, `zero)]"))
                -- putStrLn (errorableToString parsedCategory)
                isValid parsedCategory `shouldBe` True
                categoryToString (fromValid parsedCategory) `shouldBe` "`zero"
            it "should err on zero" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).decrement[`zero]"))
                -- putStrLn (errorableToString parsedCategory)
                isError parsedCategory `shouldBe` True
                error_type (head $ errors parsedCategory) `shouldBe` InvalidArgument
        describe "add" $ do
            it "should add zero and zero" $ do
                parsedCategory <- runErrorableT (executeTextual (parseCategoryString "(import $base.natural).add[(`zero, `zero)]"))
                -- putStrLn (errorableToString parsedCategory)
                isValid parsedCategory `shouldBe` True
                -- putStrLn (intercalate "\n\n" $ map categoryToString $ snd $ fromValid parsedCategory)
                categoryToString (fromValid parsedCategory) `shouldBe` "`zero"
            it "should add one and zero" $ do
                parsedCategory <- dbgEvaluate True loadTextual (parseCategoryString "(import $base.natural).add[((`successor, `zero),`zero)]")
                let result = output $ last parsedCategory
                isValid result `shouldBe` True

                -- s1 <- dbgEvaluate True loadTextual (fromValid result)
                categoryToString (fromValid result) `shouldBe` "(`successor,`zero)"
            it "should add one and one" $ do
                parsedCategory <- dbgEvaluate True loadTextual (parseCategoryString "(import $base.natural).add[((`successor, `zero),(`successor, `zero))]")
                let result = output $ last parsedCategory
                isValid result `shouldBe` True
                -- s1 <- multipleStepEvaluate 10 True loadTextual (fromValid result)
                categoryToString (fromValid result) `shouldBe` "(`successor,(`successor,`zero))"
                