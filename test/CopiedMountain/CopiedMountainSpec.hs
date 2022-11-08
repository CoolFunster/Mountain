{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainSpec (spec) where

import CopiedMountain.Data.AST 
import CopiedMountain.Parser
import CopiedMountain.Interpreter as I
import CopiedMountain.PrettyPrinter

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
    env=[]
  }


spec :: Spec
spec = do
    describe "Sum" $ do
      it "Should handle this case" $ do
        let ast = parseExpr "(x->x || 3->4)3"
        -- ast `shouldBe` Right (EMatch (EApp (ELam (PVar "x") (EVar "x")) (ELit (LInt 3))) (EApp (ELam (PLit (LInt 3)) (ELit (LInt 3))) (ELit (LInt 3))))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 3)
      it "Should handle this case" $ do
        let ast = parseExpr "(1->5 || 2->6 || 3->7)3"
        -- ast `shouldBe` Right (EMatch (EApp (ELam (PVar "x") (EVar "x")) (ELit (LInt 3))) (EApp (ELam (PLit (LInt 3)) (ELit (LInt 3))) (ELit (LInt 3))))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 7)
      it "Should handle this case 3" $ do
        let ast = parseExpr "(String -> Int :: \"hello\" -> 1) (\"hello\")"
        -- ast `shouldBe` Right (EMatch (EApp (ELam (PVar "x") (EVar "x")) (ELit (LInt 3))) (EApp (ELam (PLit (LInt 3)) (ELit (LInt 3))) (ELit (LInt 3))))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 1)
      it "Should handle this case 4" $ do
        let ast = parseExpr "(x -> y -> x) (3) (4)"
        -- ast `shouldBe` Right (EApp (EApp (ELam (PVar "x") (ELam (PVar "y") (EVar "x"))) (ELit (LInt 3))) (ELit (LInt 4)))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 3)
      it "Should handle this case 5" $ do
        let ast = parseExpr "(x ~ (Int -> Int) :: (2 -> (x 3) ||  3 -> 4)) (2)"
        ast `shouldBe` Right (EApp (ERec "x" (EAnnot (TFun TInt TInt) (EMatch (ELam (PLit (LInt 2)) (EApp (EVar "x") (ELit (LInt 3)))) (ELam (PLit (LInt 3)) (ELit (LInt 4)))))) (ELit (LInt 2)))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 4)
        