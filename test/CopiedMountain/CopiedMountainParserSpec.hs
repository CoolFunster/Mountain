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
spec = do
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
      describe "Sum" $ do
        it "Should parse simple sum" $ do
          let res = parseExpr "a -> b || c -> d"
          res `shouldBe` Right (EMatch (ELam (PVar "a") (EVar "b")) (ELam (PVar "c") (EVar "d")))
        it "Should parse simple sum with parens" $ do
          let res = parseExpr "(a -> b) || (c -> d)"
          case res of
            Left e -> error e
            Right x -> x `shouldBe` EMatch (ELam (PVar "a") (EVar "b")) (ELam (PVar "c") (EVar "d"))
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
      describe "Tuple" $ do
        it "Should parse int" $ do
          let res = parseExpr "(3)"
          res `shouldBe` Right (ELit (LInt 3))
        it "Should parse bool" $ do
          let res = parseExpr "(true)"
          res `shouldBe` Right (ELit (LBool True))
        it "Should parse tuple" $ do
          let res = parseExpr "(true, false)"
          res `shouldBe` Right (EPair (ELit (LBool True)) (ELit (LBool False)))
        it "Should parse tuple" $ do
          let res = parseExpr "(3, 4, 5, 6)"
          res `shouldBe` Right (EPair (ELit (LInt 3)) (EPair (ELit (LInt 4)) (EPair (ELit (LInt 5)) (ELit (LInt 6)))))
      describe "Call" $ do
        it "Should parse a spaced call" $ do
          let res = parseExpr "a b"
          res `shouldBe` Right (EApp (EVar "a") (EVar "b"))
        it "Should parse a close call" $ do
          let res = parseExpr "a(b)"
          res `shouldBe` Right (EApp (EVar "a") (EVar "b"))
        it "Should parse a function" $ do
          let res = parseExpr "a -> b c"
          res `shouldBe` Right (ELam (PVar "a") (EApp (EVar "b") (EVar "c")))
      describe "Let" $ do
        it "Should parse a let" $ do
          let res = parseExpr "a = b; a"
          res `shouldBe` Right (ELet "a" (EVar "b") (EVar "a"))
        it "Should parse a let in a function" $ do
          let res = parseExpr "x -> a = b; a"
          res `shouldBe` Right (ELam (PVar "x") (ELet "a" (EVar "b") (EVar "a")))
      describe "Sum" $ do
        it "Should parse a sum" $ do
          let res = parseExpr "temp<3>"
          res `shouldBe` Right (ESum "temp" (ELit (LInt 3)))
        it "Should parse a sum pattern" $ do
          let res = parseExpr "temp<x> -> x"
          res `shouldBe` Right (ELam (PSum "temp" (PVar "x")) (EVar "x"))
