{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainTypeCheckerSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Parser
import CopiedMountain.Typechecker

import Data.Map.Strict as M
import Data.Either
import Test.Hspec

-- TODO Split into respective files

spec :: Spec
spec = do
    describe "Match Call" $ do
      it "case 1" $ do
        let Right x = parseExpr "(3  ->  4|| \"s\" -> \"t\")(3)"
        let res = runTI (typeInference primitives x)
        print res
        x `shouldBe` EApp (EMatch (ELam (PLit (LInt 3)) (ELit (LInt 4))) (ELam (PLit (LString "s")) (ELit (LString "t")))) (ELit (LInt 3))
        