{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module MountainParserSpec (spec) where

import Mountain
import MountainParser
import Hash
import Data.Map.Strict as M
import Data.Either
import qualified Data.UUID as UUID
import Test.Hspec

-- TODO Split into respective files

spec :: Spec
spec = do
    describe "Literals" $ do
      it "Should parse one thing" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Literals.1_thing"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "hello")]
      it "Should parse one all" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Literals.2_all"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal All]
      it "Should parse one wildcard" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Literals.4_wildcard"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Wildcard]
      it "Should parse multiple literals" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Literals.5_multiple"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "a"),Literal (Thing "b"),Literal (Thing "c"),Literal All]
    describe "Reference" $ do
      it "Should parse a reference" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.References.1_reference"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Reference "something"]
    describe "Import" $ do
      it "Should parse an import" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Imports.1_import_ref"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Reference "something")]
      it "Should parse an import" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Imports.2_import_nested"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Select (Reference "something") ["hello"])]
    describe "Set" $ do
      it "Should parse a basic set of one elem" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Sets.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Literal (Thing "1")]]
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Sets.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse recursive sets" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Sets.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Literal (Thing "1"),Set [Literal (Thing "2")],Set [Set []]]]
      it "Should ignore ws" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Sets.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]]
    describe "Tuple" $ do
      it "Should parse a basic tuple of one elem" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Tuples.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1")]
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Tuples.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse recursive sets" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Tuples.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should ignore ws" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Tuples.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]]
    describe "Function" $ do
      it "Should parse a basic function" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Functions.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Literal (Thing "1"),Literal (Thing "2")],Function [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse a chain of functions" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Functions.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]]
      it "Should parse a mix of values within a function" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Functions.3_mixed"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Function [Literal (Thing "4"),Tuple [Literal (Thing "1"),Literal (Thing "2")],Set [Literal (Thing "3")],Reference "x"])]
    describe "Either" $ do
      it "Should parse an either" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eithers.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse an either chain" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eithers.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eithers.3_eithers_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Either [Literal (Thing "1"),Literal (Thing "2")],Either [Literal (Thing "3"),Literal (Thing "4")]]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eithers.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Literal (Thing "1"),Function [Literal (Thing "2"),Literal (Thing "3")],Literal (Thing "4")]]
    describe "Each" $ do
      it "Should parse an every" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eachs.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse an either chain" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eachs.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eachs.3_eachs_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Each [Literal (Thing "1"),Literal (Thing "2")],Each [Literal (Thing "3"),Literal (Thing "4")]]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eachs.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Literal (Thing "1"),Function [Literal (Thing "2"),Literal (Thing "3")],Literal (Thing "4")]]
      it "Should prioritize eachs over eithers" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Eachs.5_each_and_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Each [Literal (Thing "1"),Literal (Thing "2")],Each [Literal (Thing "3"),Literal (Thing "4")]]]
    describe "Scope" $ do
      it "Should parse a basic Scope" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Scopes.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1")]
      it "Should parse a multiple term scope" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Scopes.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse nested scopes" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Scopes.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse ws" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Scopes.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]
    describe "Refined" $ do
      it "Should parse refine" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Refines.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Literal (Thing "1")) (Literal (Thing "2"))]
      it "Should parse Either first" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Refines.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Either [Literal (Thing "1"),Literal (Thing "2")]) (Literal (Thing "3"))]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Refines.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Literal (Thing "1")) (Each [Literal (Thing "2"),Literal (Thing "3")])]
      it "Should parse ws" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Refines.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Literal (Thing "1")) (Literal (Thing "2")),Refine (Literal (Thing "1")) (Literal (Thing "2"))]
    describe "Unique" $ do
      it "Should parse Unique" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Uniques.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Unique Unset (Literal $ Thing "2")]
      it "Should parse Either second" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Uniques.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Unique Unset (Either [Literal (Thing "2"),Literal (Thing "3")])]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Uniques.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Unique Unset (Literal (Thing "2"))]
    describe "Call" $ do
      it "Should parse Call" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Reference "a") (Reference "b"),Call (Reference "a") (Reference "b")]
      it "Should parse Either first, call second" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Call (Literal (Thing "1")) (Literal (Thing "2")),Literal (Thing "3")]]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Reference "x") (Call (Reference "y") (Reference "z"))]
      it "Should parse call seq" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.4_sequence"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Call (Call (Call (Call (Reference "x") (Reference "y")) (Reference "z")) (Reference "a")) (Reference "b")) (Reference "c")]
      it "Should handle infix" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.5_infix"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Call (Reference "+") (Call (Call (Reference "+") (Literal (Int 1))) (Literal (Int 2)))) (Literal (Int 3))]
      it "Should handle mixfix" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Calls.6_mixfix"
        let (Right (val, env), log) = res
        val `shouldBe`  Scope [Call (Call (Call (Call (Reference "+") (Literal (Int 1))) (Literal (Int 2))) (Reference "+")) (Literal (Int 3))]
    describe "Has" $ do
      it "Should parse Has" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Hass.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "a") (Reference "b"),Has (Reference "a") (Reference "b")]
      it "Should parse Either first, has second" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Hass.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Has (Literal (Thing "1")) (Literal (Thing "2")),Literal (Thing "3")]]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Hass.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Each [Reference "x",Reference "y"]) (Reference "z")]
    describe "Bind" $ do
      it "Should parse Bind colon and equal" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Bind.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Bind (Reference "a") (Reference "b"),Bind (Reference "a") (Reference "b")]
      it "= should parse last" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Bind.2_="
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Bind (Literal (Float 1.0)) (Either [Literal (Float 2.0),Literal (Float 3.1)])]
      it ": should parse first" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Bind.3_:"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Bind (Literal (Int 1)) (Literal (Int 2)),Literal (Int 3)]]
      it "mixing = and : should be right" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Bind.4_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Bind (Bind (Reference "x") (Bind (Reference "y") (Reference "z"))) (Reference "a"),Bind (Reference "x") (Bind (Bind (Reference "y") (Reference "z")) (Reference "a"))]
    describe "Select" $ do
      it "Should parse Select" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Select.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [
          Select (Reference "a") ["b"],
          Select (Tuple [Reference "a",Reference "b"]) ["a"],
          Select (Tuple [Reference "a",Reference "b"]) ["a","b","c","d"]]
      it "Should parse select first, other second" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Select.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Reference "a",Select (Reference "b") ["c"]]]
      it "Should parse recursive selects" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Select.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Select (Reference "x") ["y","z"]]
    describe "Contexts" $ do
      it "Should parse Contexts" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Contexts.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Context (M.fromList [("x",Literal (Thing "1"))]) (Tuple [Reference "x",Literal (Thing "2")])]
      it "Should parse either first, other second" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Contexts.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Reference "a",Context (fromList [("x",Literal (Int 1))]) (Reference "x")]]
      it "Should handle recursive contexts" $ do
        res <- runMountain $ dotImportFile "Tests.Parser.Contexts.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Context (fromList [("x",Literal (Int 1)),("y",Literal (Int 2))]) (Tuple [Reference "x",Reference "y"])]
      
      -- it "Should parse recursive selects" $ do
      --   res <- runMountain $ dotImportFile "Tests.Parser.Context.3_recursive"
      --   let (Right (val, env), log) = res
      --   val `shouldBe` Select (Reference "x") ["y","z"]
    -- describe "Context" $ do

