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
    describe "Externs" $ do
      it "Should parse one thing" $ do
        res <- runMountain $ dotImportFile "ParserTests.Externs.1_thing"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Extern (Thing "hello")]
      it "Should parse one all" $ do
        res <- runMountain $ dotImportFile "ParserTests.Externs.2_all"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Reference "All"]
      it "Should parse one wildcard" $ do
        res <- runMountain $ dotImportFile "ParserTests.Externs.4_wildcard"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Wildcard]
      it "Should parse multiple literals" $ do
        res <- runMountain $ dotImportFile "ParserTests.Externs.5_multiple"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Extern (Thing "a"),Extern (Thing "b"),Extern (Thing "c"),Reference "All"]
    describe "Reference" $ do
      it "Should parse a reference" $ do
        res <- runMountain $ dotImportFile "ParserTests.References.1_reference"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Reference "something"]
    describe "Import" $ do
      it "Should parse an import" $ do
        res <- runMountain $ dotImportFile "ParserTests.Imports.1_import_ref"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Reference "something")]
      it "Should parse an import" $ do
        res <- runMountain $ dotImportFile "ParserTests.Imports.2_import_nested"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Select (Reference "something") ["hello"])]
    describe "Set" $ do
      it "Should parse a basic set of one elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Sets.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Extern (Thing "1")]]
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Sets.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse recursive sets" $ do
        res <- runMountain $ dotImportFile "ParserTests.Sets.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Extern (Thing "1"),Set [Extern (Thing "2")],Set [Set []]]]
      it "Should ignore ws" $ do
        res <- runMountain $ dotImportFile "ParserTests.Sets.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Set [Extern (Thing "1"),Extern (Thing "2"),Extern (Thing "3")]]
    describe "Literal" $ do
      it "Should parse a basic tuple of one elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Literals.0_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Extern (Thing "x"))]
      it "Should parse a basic tuple of one elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Literals.0_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Extern (Thing "x"))]
    describe "Tuple" $ do
      it "Should parse a basic tuple of one elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Tuples.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Extern (Thing "1")]]
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ dotImportFile "ParserTests.Tuples.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse recursive tuples" $ do
        res <- runMountain $ dotImportFile "ParserTests.Tuples.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Extern (Thing "1"),Tuple [Extern (Thing "2")],Tuple [Tuple []]]]
      it "Should ignore ws" $ do
        res <- runMountain $ dotImportFile "ParserTests.Tuples.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Tuple [Extern (Thing "1"),Extern (Thing "2"),Extern (Thing "3")]]
    describe "Function" $ do
      it "Should parse a basic function" $ do
        res <- runMountain $ dotImportFile "ParserTests.Functions.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Extern (Thing "1"),Extern (Thing "2")],Function [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse a chain of functions" $ do
        res <- runMountain $ dotImportFile "ParserTests.Functions.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Extern (Thing "1"),Function [Extern (Thing "2"),Function [Extern (Thing "3"),Extern (Thing "4")]]]]
      it "Should parse a mix of values within a function" $ do
        res <- runMountain $ dotImportFile "ParserTests.Functions.3_mixed"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Import (Function [Extern (Thing "4"),Function [Tuple [Extern (Thing "1"),Extern (Thing "2")],Function [Set [Extern (Thing "3")],Reference "x"]]])]
    describe "Either" $ do
      it "Should parse an either" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eithers.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse an either chain" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eithers.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Extern (Thing "1"),Either [Extern (Thing "2"),Either [Extern (Thing "3"),Extern (Thing "4")]]]]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eithers.3_eithers_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Extern (Thing "1"),Either [Function [Extern (Thing "2"),Extern (Thing "3")],Extern (Thing "4")]]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eithers.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Extern (Thing "1"),Either [Tuple [Function [Extern (Thing "2"),Extern (Thing "3")]],Extern (Thing "4")]]]
    describe "Each" $ do
      it "Should parse an every" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eachs.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse an either chain" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eachs.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Extern (Thing "1"),Each [Extern (Thing "2"),Each [Extern (Thing "3"),Extern (Thing "4")]]]]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eachs.3_eachs_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Extern (Thing "1"),Each [Function [Extern (Thing "2"),Extern (Thing "3")],Extern (Thing "4")]]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eachs.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Extern (Thing "1"),Each [Tuple [Function [Extern (Thing "2"),Extern (Thing "3")]],Extern (Thing "4")]]]
      it "Should parse each and either at same precedence" $ do
        res <- runMountain $ dotImportFile "ParserTests.Eachs.5_each_and_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Each [Extern (Thing "1"),Either [Extern (Thing "2"),Each [Extern (Thing "3"),Extern (Thing "4")]]]]
    describe "Scope" $ do
      it "Should parse a basic Scope" $ do
        res <- runMountain $ dotImportFile "ParserTests.Scopes.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Scope [Extern (Thing "1")]]
      it "Should parse a multiple term scope" $ do
        res <- runMountain $ dotImportFile "ParserTests.Scopes.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Scope [Extern (Thing "1"),Extern (Thing "2")]]
      it "Should parse nested scopes" $ do
        res <- runMountain $ dotImportFile "ParserTests.Scopes.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Scope [Extern (Thing "1"),Scope [Extern (Thing "2")],Scope [Scope []]]]
      it "Should parse ws" $ do
        res <- runMountain $ dotImportFile "ParserTests.Scopes.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Scope [Extern (Thing "1"),Extern (Thing "2"),Extern (Thing "3")]]
    describe "Refined" $ do
      it "Should parse refine" $ do
        res <- runMountain $ dotImportFile "ParserTests.Refines.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Extern (Thing "1")) (Extern (Thing "2"))]
      it "Should parse Either first" $ do
        res <- runMountain $ dotImportFile "ParserTests.Refines.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Either [Extern (Thing "1"),Extern (Thing "2")]) (Extern (Thing "3"))]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "ParserTests.Refines.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Refine (Extern (Thing "1")) (Extern (Thing "2"))) (Extern (Thing "3"))]
      it "Should parse ws" $ do
        res <- runMountain $ dotImportFile "ParserTests.Refines.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Extern (Thing "1")) (Extern (Thing "2")),Refine (Extern (Thing "1")) (Extern (Thing "2"))]
    describe "Unique" $ do
      it "Should parse Unique" $ do
        res <- runMountain $ dotImportFile "ParserTests.Uniques.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [UniqueRef (Extern $ Thing "2")]
      it "Should parse Either second" $ do
        res <- runMountain $ dotImportFile "ParserTests.Uniques.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [UniqueRef (Extern (Thing "2")),Extern (Thing "3")]]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "ParserTests.Uniques.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [UniqueRef (Tuple [UniqueRef (Extern (Thing "2"))])]
      it "Should parse eq" $ do
        res <- runMountain $ dotImportFile "ParserTests.Uniques.4_is"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Call (Reference "is") (UniqueRef (Reference "x"))) (UniqueRef (Extern (Int 3)))]
    describe "Call" $ do
      it "Should parse Call" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Reference "a") (Tuple [Reference "b"]),Call (Reference "a") (Reference "b")]
      it "Should parse Either first, call second" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Call (Extern (Thing "1")) (Extern (Thing "2")),Extern (Thing "3")]]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Reference "x") (Tuple [Call (Reference "y") (Tuple [Reference "z"])])]
      it "Should parse call seq" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.4_sequence"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Call (Call (Call (Call (Reference "x") (Tuple [Reference "y"])) (Tuple [Reference "z"])) (Tuple [Reference "a"])) (Tuple [Reference "b"])) (Tuple [Reference "c"])]
      it "Should handle infix" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.5_infix"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Call (Reference "+") (Call (Call (Reference "+") (Extern (Int 1))) (Extern (Int 2)))) (Extern (Int 3))]
      it "Should handle mixfix" $ do
        res <- runMountain $ dotImportFile "ParserTests.Calls.6_mixfix"
        let (Right (val, env), log) = res
        val `shouldBe`  Scope [Call (Call (Call (Call (Reference "+") (Extern (Int 1))) (Extern (Int 2))) (Reference "+")) (Extern (Int 3))]
    describe "Has" $ do
      it "Should parse Has" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "a") (Reference "b"),Has (Reference "a") (Reference "b")]
      it "Should parse Either first, has second" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Has (Extern (Thing "1")) (Extern (Thing "2")),Extern (Thing "3")]]
      it "Should parse nested" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "x") (Has (Reference "y") (Reference "z"))]
      it "Should parse close" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.4_close"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "a") (Reference "b")]
      it "Should parse close" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.5_binding_mix"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "a") (Def (Reference "b") (Reference "c"))]
      it "Should parse close" $ do
        res <- runMountain $ dotImportFile "ParserTests.Hass.6_calls"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Has (Set [Extern (Int 1)]) (Extern (Int 1))) (Extern (Int 2)),Has (Set [Extern (Int 1)]) (Call (Extern (Int 1)) (Extern (Int 2)))]
    describe "Define" $ do
      it "Should parse Def colon and equal" $ do
        res <- runMountain $ dotImportFile "ParserTests.Define.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Def (Reference "a") (Reference "b"),Def (Reference "a") (Reference "b")]
      it "= should parse last" $ do
        res <- runMountain $ dotImportFile "ParserTests.Define.2_="
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Def (Extern (Float 1.0)) (Either [Extern (Float 2.0),Extern (Float 3.1)])]
      it ": should parse first" $ do
        res <- runMountain $ dotImportFile "ParserTests.Define.3_:"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Def (Extern (Int 1)) (Extern (Int 2)),Extern (Int 3)]]
      it "mixing = and : should be right" $ do
        res <- runMountain $ dotImportFile "ParserTests.Define.4_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Def (Def (Reference "x") (Def (Reference "y") (Reference "z"))) (Reference "a"),Def (Reference "x") (Def (Def (Reference "y") (Reference "z")) (Reference "a"))]
    describe "Select" $ do
      it "Should parse Select" $ do
        res <- runMountain $ dotImportFile "ParserTests.Select.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [
          Select (Reference "a") ["b"],
          Select (Tuple [Reference "a",Reference "b"]) ["a"],
          Select (Tuple [Reference "a",Reference "b"]) ["a","b","c","d"]]
      it "Should parse select first, other second" $ do
        res <- runMountain $ dotImportFile "ParserTests.Select.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Reference "a",Select (Reference "b") ["c"]]]
      it "Should parse recursive selects" $ do
        res <- runMountain $ dotImportFile "ParserTests.Select.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Select (Select (Reference "x") ["y"]) ["z"]]
    describe "Contexts" $ do
      it "Should parse Contexts" $ do
        res <- runMountain $ dotImportFile "ParserTests.Contexts.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Context (M.fromList [("x",Extern (Thing "1"))]) (Tuple [Reference "x",Extern (Thing "2")])]
      it "Should parse either first, other second" $ do
        res <- runMountain $ dotImportFile "ParserTests.Contexts.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Either [Reference "a",Context (fromList [("x",Extern (Int 1))]) (Reference "x")]]
      it "Should handle recursive contexts" $ do
        res <- runMountain $ dotImportFile "ParserTests.Contexts.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Context (fromList [("x",Extern (Int 1))]) (Context (fromList [("y",Extern (Int 2))]) (Tuple [Reference "x",Reference "y"]))]
      it "Should handle multiple terms" $ do
        res <- runMountain $ dotImportFile "ParserTests.Contexts.4_multiple"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Context (fromList [("x",Extern (Int 1)),("y",Extern (Int 2))]) (Tuple [Reference "x",Reference "y"])]
      
      -- it "Should parse recursive selects" $ do
      --   res <- runMountain $ dotImportFile "ParserTests.Context.3_recursive"
      --   let (Right (val, env), log) = res
      --   val `shouldBe` Select (Reference "x") ["y","z"]
    -- describe "Context" $ do

