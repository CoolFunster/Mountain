{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainTypeCheckerSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Errors
import CopiedMountain.Parser
import CopiedMountain.Typechecker
import CopiedMountain.Context

import Data.Map.Strict as M
import Data.Either
import Test.Hspec

-- TODO Split into respective files
dummyState :: State
dummyState = State {
    changed=False,
    options=Options{
      parser=parseFile,
      repository="",
      file_ext=".mtn"},
    env=[],
    name_counter=0
  }


spec :: Spec
spec = do
    describe "Match Call" $ do
      it "case 1" $ do
        let Right x = parseExpr "(3  ->  4 || \"s\" -> \"t\")(3)"
        x `shouldBe` EApp (EMatch (ELam (PLit (LInt 3)) (ELit (LInt 4))) (ELam (PLit (LString "s")) (ELit (LString "t")))) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` TInt
      it "case 1b" $ do
        let Right x = parseExpr "(3  ->  \"s\" || \"s\" -> 4)(3)"
        x `shouldBe` EApp (EMatch (ELam (PLit (LInt 3)) (ELit (LString "s"))) (ELam (PLit (LString "s")) (ELit (LInt 4)))) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` TString
    describe "TypeAnnotations" $ do
      it "case 1" $ do
        let Right x = parseExpr "(String :: x) -> 3"
        x `shouldBe` ELam (PAnnot TString (PVar "x")) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (TFun TString TInt)
    describe "Type Calls" $ do
      it "case 1" $ do
        let Right x = parseExpr "type PairWith = a -> (a, Int); PairWith(Int) :: (3, 4)"
        x `shouldBe` ETDef "PairWith" (TFun (TVar "a") (TPair (TVar "a") TInt)) (EAnnot (TCall (TVar "PairWith") TInt) (EPair (ELit (LInt 3)) (ELit (LInt 4))))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (TPair TInt TInt)
      it "case 2" $ do
        let Right x = parseExpr "type List = a -> (()|(a,(List)(a)));((List)(Int))::((3,()))"
        -- x `shouldBe`  ETDef "List" (TFun (TVar "a") (TSum TUnit (TPair (TVar "a") (TCall (TVar "List") (TVar "a"))))) (EAnnot (TCall (TVar "List") TInt) (EPair (ELit (LInt 3)) (ELit LUnit)))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (TPair TInt TUnit)
    describe "Unify" $ do
      it "case x" $ do
        (raw_res, _) <- runWith dummyState  $ has primitives (TSum TUnit (TPair TInt (TCall (TVar "List") TInt))) TUnit
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (emptySubst, TUnit)
      it "case sum disjoint" $ do
        (raw_res, _) <- runWith dummyState  $ unify primitives (TSum TUnit TThing) (TSum TUnit TInt)
        raw_res  `shouldBe` Left (BadUnify TUnit TInt)


        