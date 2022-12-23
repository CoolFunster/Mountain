{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module CopiedMountain.CopiedMountainTypeCheckerSpec (spec) where

import CopiedMountain.Data.AST
import CopiedMountain.Data.Errors
import CopiedMountain.Parser
import CopiedMountain.Typechecker
import CopiedMountain.Context
import CopiedMountain.Hash
import CopiedMountain.PrettyPrinter

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
        x `shouldBe` EApp (EMatch (EFun (PLit (LInt 3)) (ELit (LInt 4))) (EFun (PLit (LString "s")) (ELit (LString "t")))) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TSum TInt TString)
      it "case 1b" $ do
        let Right x = parseExpr "(3  ->  \"s\" || \"s\" -> 4)(3)"
        x `shouldBe` EApp (EMatch (EFun (PLit (LInt 3)) (ELit (LString "s"))) (EFun (PLit (LString "s")) (ELit (LInt 4)))) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TSum TString TInt)
    describe "TypeAnnotations" $ do
      it "case 1" $ do
        let Right x = parseExpr "(String :: x) -> 3"
        x `shouldBe` EFun (PAnnot (ULit CAny, TString) (PVar "x")) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TFun (ULit CSingle, TString) TInt)
    describe "Type Calls" $ do
      it "case 1" $ do
        let Right x = parseExpr "type PairWith = a -> (a, Int); PairWith(Int) :: (3, 4)"
        x `shouldBe` ETDef "PairWith" (TFun (ULit CAny, TVar "a") (TPair (TVar "a") TInt)) (EAnnot (TCall (TVar "PairWith") TInt) (EPair (ELit (LInt 3)) (ELit (LInt 4))))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TPair TInt TInt)
      it "case 2" $ do
        let Right x = parseExpr "type List = a -> (()|(a,(List)(a)));((List)(Int))::((3,()))"
        -- x `shouldBe`  ETDef "List" (TFun (TVar "a") (TSum TUnit (TPair (TVar "a") (TCall (TVar "List") (TVar "a"))))) (EAnnot (TCall (TVar "List") TInt) (EPair (ELit (LInt 3)) (ELit LUnit)))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TPair TInt TUnit)
    describe "Unify/Has" $ do
      it "case x" $ do
        (raw_res, _) <- runWith dummyState  $ has primitives (TSum TUnit (TPair TInt (TCall (TVar "List") TInt))) TUnit
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (emptySubst, TUnit)
      it "case sum disjoint" $ do
        (raw_res, _) <- runWith dummyState  $ unify primitives (TSum TUnit TThing) (TSum TUnit TInt)
        raw_res  `shouldBe` Left (BadUnify TUnit TInt)
    describe "Copied" $ do
      it "should infer a copied type" $ do
        let x = parseExpr "(x,?) -> (x,x)"
        x `shouldBe` Right (EFun (PPair (PVar "x") PWildcard) (EPair (EVar "x") (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0","u1"] (TFun (UPair CAny (ULit CMany) (ULit CAny), TPair ((TVar "u0")) ((TVar "u1"))) (TPair (TVar "u0") (TVar "u0")))
    describe "Unique" $ do
      it "Should infer a literal" $ do
        let x = ELit (LInt 3)
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TInt)
      it "Should infer a unique function" $ do
        let x = EFun (PVar "x") (EVar "x")
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0"] (TFun (ULit CSingle, (TVar "u0")) (TVar "u0"))
      it "Should infer a unique function" $ do
        let x = EFun (PVar "x") (EVar "x")
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0"] (TFun (ULit CSingle, (TVar "u0")) (TVar "u0"))
      it "Should infer a pair" $ do
        let x = EPair (ELit (LInt 3)) (ELit (LFloat 3.0))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TPair TInt TFloat)
      it "Should infer a unique match" $ do
        let x = parseExpr "(x -> x || x -> x)"
        x `shouldBe` Right (EMatch (EFun (PVar "x") (EVar "x")) (EFun (PVar "x") (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u3"] (TFun (ULit CSingle, (TVar "u3")) ((TVar "u3")))
      it "Should infer a mix match" $ do
        let x = parseExpr "(x -> (x,x) || x -> (3,x))"
        x `shouldBe` Right (EMatch (EFun (PVar "x") (EPair (EVar "x") (EVar "x"))) (EFun (PVar "x") (EPair (ELit (LInt 3)) (EVar "x"))))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TFun (ULit CMany, TInt) (TPair TInt TInt))
      it "Should infer a app" $ do
        let x = parseExpr "(3 -> 5)(3)"
        x `shouldBe` Right (EApp (EFun (PLit (LInt 3)) (ELit (LInt 5))) (ELit (LInt 3)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] TInt
      it "Should infer a app" $ do
        let x = parseExpr "(3 -> 5)(3)"
        x `shouldBe` Right (EApp (EFun (PLit (LInt 3)) (ELit (LInt 5))) (ELit (LInt 3)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] TInt
      it "Should infer a unique arg" $ do
        let x = parseExpr "(x -> 5)"
        x `shouldBe` Right (EFun (PVar "x") (ELit (LInt 5)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0"] (TFun (ULit CSingle, (TVar "u0")) TInt)
      it "Should infer a unique app" $ do
        let x = parseExpr "x -> (3,x)"
        x `shouldBe` Right (EFun (PVar "x") (EPair (ELit (LInt 3)) (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0"] (TFun (ULit CSingle, (TVar "u0")) (TPair TInt (TVar "u0")))
      it "Should infer this unique type" $ do
        let x = parseExpr "(3,3)"
        x `shouldBe` Right (EPair (ELit (LInt 3)) (ELit (LInt 3)))
        let x' = (fromRight (error "") x)
        prettyExp x'  `shouldBe` "(3,3)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TPair TInt TInt)
      it "Should handle unique input output" $ do
        let x = parseExpr "x -> x"
        x `shouldBe` Right (EFun ((PVar "x")) ((EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "x->x"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme ["u0"] (TFun (ULit CSingle, (TVar "u0")) (TVar "u0"))
      it "Should handle unique pattern types" $ do
        let x = parseExpr "(+Int :: x) -> (x,x)"
        x `shouldBe` Right (EFun (PAnnot (ULit CMany, TInt) (PVar "x")) (EPair (EVar "x") (EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "(+Int::x)->(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) -> res `shouldBe` Scheme [] (TFun (ULit CMany,TInt) (TPair TInt TInt))
      it "Should handle unique pattern types" $ do
        let x = parseExpr "(?Int :: x) -> (x,x)"
        x `shouldBe` Right (EFun (PAnnot (ULit CSingle, TInt) (PVar "x")) (EPair (EVar "x") (EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "(?Int::x)->(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        raw_res `shouldBe` Left (BadUsage (ULit CSingle) (ULit CMany))
      it "Should handle unique type annotations" $ do
        let x = parseExpr "(+Int, String) -> (Int, Int) :: (x,?) -> (x,x)"
        x `shouldBe` Right (EAnnot (TFun (UPair CAny (ULit CMany) (ULit CAny), TPair TInt TString) (TPair TInt TInt)) (EFun (PPair (PVar "x") PWildcard) (EPair (EVar "x") (EVar "x"))))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "((+Int, String) -> (Int,Int))::((x,?)->(x,x))"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TFun (UPair CAny (ULit CMany) (ULit CAny), TPair TInt TString) (TPair TInt TInt))
      it "Should handle lets" $ do
        let x = parseExpr "(def x = 3; (x,x))"
        x `shouldBe` Right (EAnnot (TFun (UPair CAny (ULit CMany) (ULit CAny), TPair TInt TString) (TPair TInt TInt)) (EFun (PPair (PVar "x") PWildcard) (EPair (EVar "x") (EVar "x"))))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "((+Int, String) -> (Int,Int))::((x,?)->(x,x))"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TFun (UPair CAny (ULit CMany) (ULit CAny), TPair TInt TString) (TPair TInt TInt))
    describe "lets" $ do
      it "Should handle polymorphic functions" $ do
        let x = parseExpr "def id = x -> x; (id(3), id(\"s\"))"
        x `shouldBe` Right (ELet (PVar "id") (EFun (PVar "x") (EVar "x")) (EPair (EApp (EVar "id") (ELit (LInt 3))) (EApp (EVar "id") (ELit (LString "s")))))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "id=x->x;((id)(3),(id)(\"s\"))"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` Scheme [] (TPair TInt TString)



        