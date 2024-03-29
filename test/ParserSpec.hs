{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module ParserSpec (spec) where

import AST
import Parser
import Hash

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
          let res = parseExpr "someVar"
          res `shouldBe` Right (EVar "someVar")
        it "Should parse quoted var" $ do
          let res = parseExpr "someVar'"
          res `shouldBe` Right (EVar "someVar'")
      describe "Lambda" $ do
        it "Should parse simple function" $ do
          let res = parseExpr "a -> b"
          res `shouldBe` Right (EFun (PVar "a") (EVar "b"))
        it "Should parse long chain function" $ do
          let res = parseExpr "a -> b -> c -> d -> e"
          res `shouldBe` Right (EFun (PVar "a") (EFun (PVar "b") (EFun (PVar "c") (EFun (PVar "d") (EVar "e")))))
        it "Should parse close function" $ do
          let res = parseExpr "a->b"
          res `shouldBe` Right (EFun (PVar "a") (EVar "b"))
      describe "Sum" $ do
        it "Should parse simple sum" $ do
          let res = parseExpr "a -> b || c -> d"
          case res of
            Right x -> x `shouldBe` EMatch (EFun (PVar "a") (EVar "b")) (EFun (PVar "c") (EVar "d"))
            Left e -> error e
        it "Should parse simple sum with parens" $ do
          let res = parseExpr "(a -> b) || (c -> d)"
          case res of
            Left e -> error e
            Right x -> x `shouldBe` EMatch (EFun (PVar "a") (EVar "b")) (EFun (PVar "c") (EVar "d"))
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
          res `shouldBe` Right (EFun (PVar "a") (EApp (EVar "b") (EVar "c")))
      describe "Let" $ do
        it "Should parse a let" $ do
          let res = parseExpr "let a = b; a"
          res `shouldBe` Right (ELet (PVar "a") (EVar "b") (EVar "a"))
        it "Should parse a let in a function" $ do
          let res = parseExpr "x -> let a = b; a"
          case res of
            Left e -> error e
            Right x -> x `shouldBe` EFun (PVar "x") (ELet (PVar "a") (EVar "b") (EVar "a"))
        it "Should parse this let" $ do
          let res = parseExpr "let id = x -> x; id"
          res `shouldBe` Right (ELet (PVar "id") (EFun (PVar "x") (EVar "x")) (EVar "id"))
        it "Should parse this let2" $ do
          let res = parseExpr "let id = x -> x; (id(3), id(\"s\"))"
          res `shouldBe` Right (ELet (PVar "id") (EFun (PVar "x") (EVar "x")) (EPair (EApp (EVar "id") (ELit (LInt 3))) (EApp (EVar "id") (ELit (LString "s")))))
        it "Should parse usage lets" $ do
          let res = parseExprWith pLet "let (?Int :: x) = 3; x"
          case res of
            Left s -> error s
            Right exp -> exp `shouldBe` ELet (PAnnot (TUsage CSingle TInt) (PVar "x")) (ELit (LInt 3)) (EVar "x")
      describe "Label" $ do
        it "Should parse a label" $ do
          let res = parseExpr "temp:3"
          res `shouldBe` Right (ELabel "temp" (ELit (LInt 3)))
        it "Should parse a label foo" $ do
          let res = parseExpr "temp:x -> x"
          res `shouldBe` Right (EFun (PLabel "temp" (PVar "x")) (EVar "x"))
      describe "Annot" $ do
        it "should parse a basic type" $ do
          let res = parseExprWith pType "Int"
          res `shouldBe` Right TInt
        it "should parse a basic type foo" $ do
          let res = parseExprWith pType "Int -> String"
          res `shouldBe` Right (TFun TInt TString)
        it "should parse an Annot" $ do
          let res = parseExpr "Int :: 3"
          res `shouldBe` Right (EAnnot TInt (ELit (LInt 3)))
        it "should parse an Annot foo" $ do
          let res = parseExpr "Int -> String :: 3"
          res `shouldBe` Right (EAnnot (TFun TInt TString) (ELit (LInt 3)))
        it "should parse a nested Annot" $ do
          let res = parseExpr "Int -> String :: Int :: 3"
          res `shouldBe` Right (EAnnot (TFun TInt TString) (EAnnot TInt (ELit (LInt 3))))
        it "should parse a pattern annotation" $ do
          let res = parseExpr "(Int :: x) -> x"
          case res of
            Left s -> error s
            Right exp -> exp `shouldBe` EFun (PAnnot TInt (PVar "x")) (EVar "x")
      describe "Recursion" $ do
        it "should parse a recursive function" $ do
          let res = parseExpr "x ~ (Int -> Int) :: 3 -> (x 3)"
          res `shouldBe` Right (ERec "x" (EAnnot (TFun TInt TInt) (EFun (PLit (LInt 3)) (EApp (EVar "x") (ELit (LInt 3))))))
      describe "TypeDefs" $ do
        it "should handle typedefs" $ do
          let res = parseExpr "x ~ (Int -> Int) :: 3 -> (x 3)"
          res `shouldBe` Right (ERec "x" (EAnnot (TFun TInt TInt) (EFun (PLit (LInt 3)) (EApp (EVar "x") (ELit (LInt 3))))))
        it "should handle simple typedef" $ do
          let res = parseExpr "Int :: x"
          res `shouldBe` Right (EAnnot TInt (EVar "x"))
      describe "Struct" $ do
        it "should handle a basic struct" $ do
          let res = parseExpr "struct { expr id = x -> x; }"
          case res of
            Left s -> error s
            Right exp -> exp `shouldBe` EStruct [MExpr "id" (EFun (PVar "x") (EVar "x"))]
        it "should handle a multiple statements" $ do
          let res = parseExpr "\
            \struct {\ 
                \expr id = x -> x; \
                \expr id2 = y -> y; \
              \}"
          case res of
            Left s -> error s
            Right exp -> exp `shouldBe` EStruct [MExpr "id" (EFun (PVar "x") (EVar "x")),MExpr "id2" (EFun (PVar "y") (EVar "y"))]
