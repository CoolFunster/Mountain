{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module TypeCheckerSpec (spec) where

import AST
import Errors
import Parser
import Typechecker
import Context
import Hash
import PrettyPrinter

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
            res `shouldBe` AbstractType [] (TSum TInt TString)
      it "case 1b" $ do
        let Right x = parseExpr "(3  ->  \"s\" || \"s\" -> 4)(3)"
        x `shouldBe` EApp (EMatch (EFun (PLit (LInt 3)) (ELit (LString "s"))) (EFun (PLit (LString "s")) (ELit (LInt 4)))) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TSum TString TInt)
    describe "TypeAnnotations" $ do
      it "case 1" $ do
        let Right x = parseExpr "(String :: x) -> 3"
        x `shouldBe` EFun (PAnnot TString (PVar "x")) (ELit (LInt 3))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TFun (TUsage CSingle TString) TInt)
    describe "Type Calls" $ do
      it "case 1" $ do
        let Right x = parseExpr "type PairWith = a -> (a, Int); PairWith(Int) :: (3, 4)"
        x `shouldBe` ETDef "PairWith" (TFun (TVar "a") (TPair (TVar "a") TInt)) (EAnnot (TCall (TVar "PairWith") TInt) (EPair (ELit (LInt 3)) (ELit (LInt 4))))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TInt)
      it "case 2" $ do
        let Right x = parseExpr "type List = a -> (()|(a,(List)(a)));((List)(Int))::((3,()))"
        -- x `shouldBe`  ETDef "List" (TFun (TVar "a") (TSum TUnit (TPair (TVar "a") (TCall (TVar "List") (TVar "a"))))) (EAnnot (TCall (TVar "List") TInt) (EPair (ELit (LInt 3)) (ELit LUnit)))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TUnit)
    describe "Unify/Has" $ do
      it "case x" $ do
        (raw_res, _) <- runWith dummyState  $ has primitives (TSum TUnit (TPair TInt (TCall (TVar "List") TInt))) TUnit
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` (emptyTEnv, TUnit)
      it "case sum disjoint" $ do
        (raw_res, _) <- runWith dummyState  $ unify primitives (TSum TUnit TThing) (TSum TUnit TInt)
        raw_res  `shouldBe` Left (BadUnify TUnit TInt)
    describe "Copied" $ do
      it "should infer a copied type" $ do
        let x = parseExpr "(x,_) -> (x,x)"
        x `shouldBe` Right (EFun (PPair (PVar "x") PWildcard) (EPair (EVar "x") (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0","u1"] (TFun (TPair (TUsage CMany (TVar "u0")) (TVar "u1")) (TPair (TVar "u0") (TVar "u0")))
    describe "Unique" $ do
      it "Should infer a literal" $ do
        let x = ELit (LInt 3)
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TInt)
      it "Should infer a unique function" $ do
        let x = EFun (PVar "x") (EVar "x")
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0"] (TFun (TUsage CSingle (TVar "u0")) (TVar "u0"))
      it "Should infer a unique function" $ do
        let x = EFun (PVar "x") (EVar "x")
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0"] (TFun (TUsage CSingle (TVar "u0")) (TVar "u0"))
      it "Should infer a pair" $ do
        let x = EPair (ELit (LInt 3)) (ELit (LFloat 3.0))
        (raw_res, _) <- runWith dummyState (typeInference primitives x)
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TFloat)
      it "Should infer a unique match" $ do
        let x = parseExpr "(x -> x || x -> x)"
        x `shouldBe` Right (EMatch (EFun (PVar "x") (EVar "x")) (EFun (PVar "x") (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u3"] (TFun (TUsage CSingle (TVar "u3")) ((TVar "u3")))
      it "Should infer a mix match" $ do
        let x = parseExpr "(String :: x -> (x,x) || Int :: x -> (3,x))"
        x `shouldBe` Right (EMatch (EFun (PAnnot TString (PVar "x")) (EPair (EVar "x") (EVar "x"))) (EFun (PAnnot TInt (PVar "x")) (EPair (ELit (LInt 3)) (EVar "x"))))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TFun (TSum (TUsage CMany TString) (TUsage CSingle TInt)) (TSum (TPair TString TString) (TPair TInt TInt)))
      it "Should infer a mix match usage" $ do
        let x = parseExpr "(Int :: x -> (x,x) || Int :: x -> (3,x))"
        x `shouldBe` Right (EMatch (EFun (PAnnot TInt (PVar "x")) (EPair (EVar "x") (EVar "x"))) (EFun (PAnnot TInt (PVar "x")) (EPair (ELit (LInt 3)) (EVar "x"))))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TFun (TSum (TUsage CMany TInt) (TUsage CSingle TInt)) (TPair TInt TInt))
      it "Should infer a app" $ do
        let x = parseExpr "(3 -> 5)(3)"
        x `shouldBe` Right (EApp (EFun (PLit (LInt 3)) (ELit (LInt 5))) (ELit (LInt 3)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] TInt
      it "Should infer a app" $ do
        let x = parseExpr "(3 -> 5)(3)"
        x `shouldBe` Right (EApp (EFun (PLit (LInt 3)) (ELit (LInt 5))) (ELit (LInt 3)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] TInt
      it "Should infer a unique arg" $ do
        let x = parseExpr "(x -> 5)"
        x `shouldBe` Right (EFun (PVar "x") (ELit (LInt 5)))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0"] (TFun (TUsage CSingle (TVar "u0")) TInt)
      it "Should infer a unique app" $ do
        let x = parseExpr "x -> (3,x)"
        x `shouldBe` Right (EFun (PVar "x") (EPair (ELit (LInt 3)) (EVar "x")))
        (raw_res, _) <- runWith dummyState (typeInference primitives (fromRight (error "") x))
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0"] (TFun (TUsage CSingle (TVar "u0")) (TPair TInt (TVar "u0")))
      it "Should infer this unique type" $ do
        let x = parseExpr "(3,3)"
        x `shouldBe` Right (EPair (ELit (LInt 3)) (ELit (LInt 3)))
        let x' = (fromRight (error "") x)
        prettyExp x'  `shouldBe` "(3,3)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TInt)
      it "Should handle unique input output" $ do
        let x = parseExpr "x -> x"
        x `shouldBe` Right (EFun ((PVar "x")) ((EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "fn x->x"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType ["u0"] (TFun (TUsage CSingle (TVar "u0")) (TVar "u0"))
      it "Should handle unique pattern types" $ do
        let x = parseExpr "(+Int :: x) -> (x,x)"
        x `shouldBe` Right (EFun (PAnnot (TUsage CMany TInt) (PVar "x")) (EPair (EVar "x") (EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "fn (+Int::x)->(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) -> res `shouldBe` AbstractType [] (TFun (TUsage CMany TInt) (TPair TInt TInt))
      it "Should handle unique pattern types" $ do
        let x = parseExpr "(?Int :: x) -> (x,x)"
        x `shouldBe` Right (EFun (PAnnot (TUsage CSingle TInt) (PVar "x")) (EPair (EVar "x") (EVar "x")))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "fn (?Int::x)->(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        raw_res `shouldBe` Left (BadHasUseCount CSingle CMany)
      it "Should handle unique type annotations" $ do
        let x = parseExpr "(+Int, String) -> (Int, Int) :: ((x,_) -> (x,x))"
        x `shouldBe` Right (EAnnot (TFun (TPair (TUsage CMany TInt) TString) (TPair TInt TInt)) (EFun (PPair (PVar "x") PWildcard) (EPair (EVar "x") (EVar "x"))))
        let x' = (fromRight (error "") x)
        prettyExp x' `shouldBe` "((+Int,String) -> (Int,Int))::(fn (x,_)->(x,x))"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TFun (TPair (TUsage CMany TInt) TString) (TPair TInt TInt))
      it "Should handle pattern lets error" $ do
        let x = parseExpr "let (?Int :: x) = 3; (x,x)"
        case x of
          Left e -> error e
          Right res -> res `shouldBe` (ELet (PAnnot (TUsage CSingle TInt) (PVar "x")) (ELit (LInt 3)) (EPair (EVar "x") (EVar "x")))
        let x' = fromRight (error "") x
        prettyExp x' `shouldBe` "(?Int::x)=3;(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        raw_res `shouldBe` Left (BadHasUseCount  CSingle CMany)
      it "Should handle pattern lets correct" $ do
        let x = parseExpr "let (+Int :: x) = 3; (x,x)"
        x `shouldBe` Right (ELet (PAnnot (TUsage CMany TInt) (PVar "x")) (ELit (LInt 3)) (EPair (EVar "x") (EVar "x")))
        let x' = fromRight (error "") x
        prettyExp x' `shouldBe` "(+Int::x)=3;(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TInt)
      it "Should handle patter lets inferred" $ do
        let x = parseExpr "let (Int :: x) = 3; (x,x)"
        x `shouldBe` Right (ELet (PAnnot TInt (PVar "x")) (ELit (LInt 3)) (EPair (EVar "x") (EVar "x")))
        let x' = fromRight (error "") x
        prettyExp x' `shouldBe` "(Int::x)=3;(x,x)"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TInt)
    describe "lets" $ do
      it "Should handle polymorphic functions" $ do
        let x = parseExprWith pLet "let id = x -> x; (id(3), id(\"s\"))"
        case x of
          Left e -> error e
          Right res ->
            res `shouldBe` ELet (PVar "id") (EFun (PVar "x") (EVar "x")) (EPair (EApp (EVar "id") (ELit (LInt 3))) (EApp (EVar "id") (ELit (LString "s"))))
        let x' = fromRight (error "") x
        prettyExp x' `shouldBe` "id=fn x->x;((id)(3),(id)(\"s\"))"
        (raw_res, _) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TPair TInt TString)
    describe "structs" $ do
      it "Should typecheck simple struct" $ do
        let x = parseExpr "interface {decl anInt = Int;} :: struct {expr anInt = 3;}"
        x `shouldBe` Right (EAnnot (TInterface [MDecl "anInt" TInt]) (EStruct [MExpr "anInt" (ELit (LInt 3))]))
        let x' = fromRight (error "") x
        prettyExp x' `shouldBe` "(interface {decl anInt = Int;})::(struct {expr anInt = 3;})"
        (raw_res, log) <- runWith dummyState (typeInference primitives x')
        case raw_res of
          Left e -> error (show e)
          Right (res, _) ->
            res `shouldBe` AbstractType [] (TInterface [MDecl "anInt" TInt])



        