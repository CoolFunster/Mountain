{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainSpec (spec) where

import CopiedMountain.Data.AST 
import CopiedMountain.Parser
import CopiedMountain.Interpreter as I

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
        let ast = parseExpr "(x->x)3 | (3->3)3"
        ast `shouldBe` Right (ESum (EApp (ELam (PVar "x") (EVar "x")) (ELit (LInt 3))) (EApp (ELam (PLit (LInt 3)) (ELit (LInt 3))) (ELit (LInt 3))))
        (Right (res, state), log) <- runWith initialState $ I.evaluate (Just 20) (fromRight (error "") ast)
        res `shouldBe` ELit (LInt 3)
        