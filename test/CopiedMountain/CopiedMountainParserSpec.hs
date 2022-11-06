{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainParserSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Parser

import Data.Map.Strict as M
import Data.Either
import Test.Hspec

-- TODO Split into respective files

spec :: Spec
spec = parallel $ do
    describe "Exp" $ do
      describe "Var" $ do
        it "Should parse Var" $ do
          let res = parseExpr "x"
          res `shouldBe` Right (EVar "x")
        it "Should parse underscore var" $ do
          let res = parseExpr "some_var"
          res `shouldBe` Right (EVar "some_var")
        it "Should parse quoted var" $ do
          let res = parseExpr "some_var'"
          res `shouldBe` Right (EVar "some_var'")
      describe "Lambda" $ do
        it "Should parse simple function" $ do
          let res = parseExpr "a -> b"
          res `shouldBe` Right (ELam (PVar "a") (EVar "b"))
        it "Should parse long chain function" $ do
          let res = parseExpr "a -> b -> c -> d -> e"
          res `shouldBe` Right (ELam (PVar "a") (ELam (PVar "b") (ELam (PVar "c") (ELam (PVar "d") (EVar "e")))))  
        it "Should parse close function" $ do
          let res = parseExpr "a->b"
          res `shouldBe` Right (ELam (PVar "a") (EVar "b"))
      describe "Literal" $ do
        it "Should parse int" $ do
          let res = parseExpr "3"
          res `shouldBe` Right (ELit (LInt 3))
        it "Should parse bool" $ do
          let res = parseExpr "true"
          res `shouldBe` Right (ELit (LBool True))
      describe "Pattern" $ do
        it "Should parse var" $ do
          let res = parseExprWith pPattern "x"
          res `shouldBe` Right (PVar "x")
      describe "Parens" $ do
        it "Should parse int" $ do
          let res = parseExpr "(3)"
          res `shouldBe` Right (ELit (LInt 3))
        it "Should parse bool" $ do
          let res = parseExpr "(true)"
          res `shouldBe` Right (ELit (LBool True))
      describe "Call" $ do
        it "Should parse a spaced call" $ do
          let res = parseExpr "a b"
          res `shouldBe` Right (EApp (EVar "a") (EVar "b"))
        it "Should parse a close call" $ do
          let res = parseExpr "a(b)"
          res `shouldBe` Right (EApp (EVar "a") (EVar "b"))
      describe "Let" $ do
        it "Should parse a let" $ do
          let res = parseExpr "a = b; a"
          res `shouldBe` Right (ELet "a" (EVar "b") (EVar "a"))
        it "Should parse a let in a function" $ do
          let res = parseExpr "x -> a = b; a"
          res `shouldBe` Right (ELam (PVar "x") (ELet "a" (EVar "b") (EVar "a")))
