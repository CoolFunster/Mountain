{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainTypeCheckerSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Errors
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
        let Right x = parseExpr "(3  ->  4 || \"s\" -> \"t\")(3)"
        x `shouldBe` EApp (EMatch (ELam (PLit (LInt 3)) (ELit (LInt 4))) (ELam (PLit (LString "s")) (ELit (LString "t")))) (ELit (LInt 3))
        let (res, _) = runTI (typeInference primitives x)
        res `shouldBe` Right TInt
      it "case 1b" $ do
        let Right x = parseExpr "(3  ->  \"s\" || \"s\" -> 4)(3)"
        x `shouldBe` EApp (EMatch (ELam (PLit (LInt 3)) (ELit (LString "s"))) (ELam (PLit (LString "s")) (ELit (LInt 4)))) (ELit (LInt 3))
        let (res, _) = runTI (typeInference primitives x)
        res `shouldBe` Right TString
    describe "TypeAnnotations" $ do
      it "case 1" $ do
        let Right x = parseExpr "(String :: x) -> 3"
        x `shouldBe` ELam (PAnnot TString (PVar "x")) (ELit (LInt 3))
        let (res, _) = runTI (typeInference primitives x)
        res `shouldBe` Right (TFun TString TInt)
    describe "Type Calls" $ do
      it "case 1" $ do
        let Right x = parseExpr "type PairWith = a -> (a, Int); PairWith(Int) :: (3, 4)"
        x `shouldBe` ETDef "PairWith" (TFun (TVar "a") (TPair (TVar "a") TInt)) (EAnnot (TCall (TVar "PairWith") TInt) (EPair (ELit (LInt 3)) (ELit (LInt 4))))
        let (res, _) = runTI (typeInference primitives x)
        res `shouldBe` Right (TPair TInt TInt)
      it "case 2" $ do
        let Right x = parseExpr "type List = a -> (()|(a,(List)(a)));((List)(Int))::((3,()))"
        -- x `shouldBe`  ETDef "List" (TFun (TVar "a") (TSum TUnit (TPair (TVar "a") (TCall (TVar "List") (TVar "a"))))) (EAnnot (TCall (TVar "List") TInt) (EPair (ELit (LInt 3)) (ELit LUnit)))
        let (res, _) = runTI (typeInference primitives x)
        res `shouldBe` Right (TPair TInt TUnit)
    describe "Unify" $ do
      it "case x" $ do
        let (res, _) = runTI $ has primitives (TSum TUnit (TPair TInt (TCall (TVar "List") TInt))) TUnit
        res `shouldBe` Right (emptySubst, TUnit)
      it "case sum disjoint" $ do
        let (res, _) = runTI $ unify primitives (TSum TUnit TThing) (TSum TUnit TInt)
        res `shouldBe` Left (BadUnify TUnit TInt)


        