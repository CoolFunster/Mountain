{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Context
import CopiedMountain.Parser
import CopiedMountain.Interpreter as I
import CopiedMountain.PrettyPrinter
import CopiedMountain.Hash

import Data.Map.Strict as M
import Data.Either
import Test.Hspec

-- TODO Split into respective files
initialState :: State
initialState = State {
    changed=False,
    options=Options{
      parser=parseFile,
      repository="/home/mpriam/git/mtpl_language/Repository",
      file_ext=".mtn"},
    env=[],
    name_counter=0
  }

process :: (Maybe Int) -> Exp -> ContextT IO Exp
process steps x = do
  vx <- preprocess x
  I.evaluate steps vx

spec :: Spec
spec = do
    describe "uniquify" $ do
      it "should handle unique lets" $ do
        let ast = parseExpr "def x *= 3; x"
        ast `shouldBe` Right (EULet (PVar "x") (ELit (LInt 3)) (EVar "x"))
        res <- uniquify (fromRight (error "") ast)
        res `shouldBe` EULet (PVar "x") (ELit (LInt 3)) (EVar "x")
    describe "Sum" $ do
      it "Should handle this case" $ do
        let ast = parseExpr "(x -> x || 3 -> 4)3"
        ast `shouldBe` Right (EApp (EMatch (EULam (PVar "x") (EVar "x")) (EULam (PLit (LInt 3)) (ELit (LInt 4)))) (ELit (LInt 3)))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            res `shouldBe` ELit (LInt 3)
      it "Should handle this case" $ do
        let ast = parseExpr "(1->5 || 2->6 || 3->7)3"
        ast `shouldBe` Right (EApp (EMatch (EULam (PLit (LInt 1)) (ELit (LInt 5))) (EMatch (EULam (PLit (LInt 2)) (ELit (LInt 6))) (EULam (PLit (LInt 3)) (ELit (LInt 7))))) (ELit (LInt 3)))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            res `shouldBe` ELit (LInt 7)
      it "Should handle this case 3" $ do
        let ast = parseExpr "(String -> Int :: \"hello\" -> 1) (\"hello\")"
        ast `shouldBe` Right (EApp (EAnnot (TUFun TString TInt) (EULam (PLit (LString "hello")) (ELit (LInt 1)))) (ELit (LString "hello")))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            res `shouldBe` ELit (LInt 1)
      it "Should handle this case 4" $ do
        let ast = parseExpr "(x -> y -> x) (3) (4)"
        ast `shouldBe` Right (EApp (EApp (EULam (PVar "x") (EULam (PVar "y") (EVar "x"))) (ELit (LInt 3))) (ELit (LInt 4)))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            res `shouldBe` ELit (LInt 3)
      it "Should handle this case 5" $ do
        let ast = parseExpr "(x ~ (Int -> Int) :: (2 -> (x 3) ||  3 -> 4)) (2)"
        ast `shouldBe` Right (EApp (ERec "x" (EAnnot (TUFun TInt TInt) (EMatch (EULam (PLit (LInt 2)) (EApp (EVar "x") (ELit (LInt 3)))) (EULam (PLit (LInt 3)) (ELit (LInt 4)))))) (ELit (LInt 2)))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            res `shouldBe` ELit (LInt 4)
      it "Should handle this case 6" $ do
        let ast = parseExpr "def x *= *3; x"
        ast `shouldBe` Right (EULet (PVar "x") (EUnique Unset (ELit (LInt 3))) (EVar "x"))
        (raw_res, log) <- runWith initialState $ process (Just 20) (fromRight (error "") ast)
        case raw_res of
          Left e -> error $ prettyError e
          Right (res, state) ->
            prettyExp res `shouldBe` "*3"
        