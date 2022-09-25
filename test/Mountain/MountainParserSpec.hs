{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Mountain.MountainParserSpec (spec) where

import Mountain.Mountain
import Mountain.MountainParser
import Data.Map.Strict as M

import Test.Hspec

-- TODO Split into respective files

spec :: Spec
spec = do
    describe "Literals" $ do
      it "Should parse one thing" $ do
        res <- runMountain $ importer "TestParser.Literals.1_thing"
        let (Right (val, env), log) = res
        val `shouldBe` Literal (Thing "hello")
      it "Should parse one all" $ do
        res <- runMountain $ importer "TestParser.Literals.2_all"
        let (Right (val, env), log) = res
        val `shouldBe` Literal All
      it "Should parse one star" $ do
        res <- runMountain $ importer "TestParser.Literals.3_star"
        let (Right (val, env), log) = res
        val `shouldBe` Literal Star
      it "Should parse one wildcard" $ do
        res <- runMountain $ importer "TestParser.Literals.4_wildcard"
        let (Right (val, env), log) = res
        val `shouldBe` Literal Wildcard
      it "Should parse multiple literals" $ do
        res <- runMountain $ importer "TestParser.Literals.5_multiple"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "a"),Literal (Thing "b"),Literal (Thing "c"),Literal Star,Literal All]
    describe "Reference" $ do
      it "Should parse a reference" $ do
        res <- runMountain $ importer "TestParser.References.1_reference"
        let (Right (val, env), log) = res
        val `shouldBe` Reference "something"
    describe "Import" $ do
      it "Should parse an import" $ do
        res <- runMountain $ importer "TestParser.Imports.1_import_ref"
        let (Right (val, env), log) = res
        val `shouldBe` Import (Reference "something")
      it "Should parse an import" $ do
        res <- runMountain $ importer "TestParser.Imports.2_import_nested"
        let (Right (val, env), log) = res
        val `shouldBe` Import (Select (Reference "something") ["hello"])
    describe "Set" $ do
      it "Should parse a basic set of one elem" $ do
        res <- runMountain $ importer "TestParser.Sets.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Set [Literal (Thing "1")]
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ importer "TestParser.Sets.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Set [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse recursive sets" $ do
        res <- runMountain $ importer "TestParser.Sets.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Set [Literal (Thing "1"),Set [Literal (Thing "2")],Set [Set []]]
      it "Should ignore ws" $ do
        res <- runMountain $ importer "TestParser.Sets.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Set [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]
    describe "Tuple" $ do
      it "Should parse a basic tuple of one elem" $ do
        res <- runMountain $ importer "TestParser.Tuples.1_basic_set"
        let (Right (val, env), log) = res
        val `shouldBe` Literal (Thing "1")
      it "Should parse a basic set of many elem" $ do
        res <- runMountain $ importer "TestParser.Tuples.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Tuple [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse recursive sets" $ do
        res <- runMountain $ importer "TestParser.Tuples.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Tuple [Literal (Thing "1"),Literal (Thing "2"),Tuple []]
      it "Should ignore ws" $ do
        res <- runMountain $ importer "TestParser.Tuples.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Tuple [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]
    describe "Function" $ do
      it "Should parse a basic function" $ do
        res <- runMountain $ importer "TestParser.Functions.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Function [Literal (Thing "1"),Literal (Thing "2")],Function [Literal (Thing "1"),Literal (Thing "2")]]
      it "Should parse a chain of functions" $ do
        res <- runMountain $ importer "TestParser.Functions.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Function [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]
      it "Should parse a mix of values within a function" $ do
        res <- runMountain $ importer "TestParser.Functions.3_mixed"
        let (Right (val, env), log) = res
        val `shouldBe` Import (Function [Literal (Thing "4"),Tuple [Literal (Thing "1"),Literal (Thing "2")],Set [Literal (Thing "3")],Reference "x"])
    describe "Either" $ do
      it "Should parse an either" $ do
        res <- runMountain $ importer "TestParser.Eithers.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse an either chain" $ do
        res <- runMountain $ importer "TestParser.Eithers.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ importer "TestParser.Eithers.3_eithers_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Function [Either [Literal (Thing "1"),Literal (Thing "2")],Either [Literal (Thing "3"),Literal (Thing "4")]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ importer "TestParser.Eithers.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Literal (Thing "1"),Function [Literal (Thing "2"),Literal (Thing "3")],Literal (Thing "4")]
    describe "Each" $ do
      it "Should parse an every" $ do
        res <- runMountain $ importer "TestParser.Eachs.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Each [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse an either chain" $ do
        res <- runMountain $ importer "TestParser.Eachs.2_chain"
        let (Right (val, env), log) = res
        val `shouldBe` Each [Literal (Thing "1"),Literal (Thing "2"), Literal (Thing "3"), Literal (Thing "4")]
      it "Should parse eithers smaller than functions" $ do
        res <- runMountain $ importer "TestParser.Eachs.3_eachs_and_fns"
        let (Right (val, env), log) = res
        val `shouldBe` Function [Each [Literal (Thing "1"),Literal (Thing "2")],Each [Literal (Thing "3"),Literal (Thing "4")]]
      it "Should parse tupled function inside" $ do
        res <- runMountain $ importer "TestParser.Eachs.4_parenthesis"
        let (Right (val, env), log) = res
        val `shouldBe` Each [Literal (Thing "1"),Function [Literal (Thing "2"),Literal (Thing "3")],Literal (Thing "4")]
      it "Should prioritize eachs over eithers" $ do
        res <- runMountain $ importer "TestParser.Eachs.5_each_and_either"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Each [Literal (Thing "1"),Literal (Thing "2")],Each [Literal (Thing "3"),Literal (Thing "4")]]
    describe "Scope" $ do
      it "Should parse a basic Scope" $ do
        res <- runMountain $ importer "TestParser.Scopes.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Literal (Thing "1")
      it "Should parse a multiple term scope" $ do
        res <- runMountain $ importer "TestParser.Scopes.2_many_elem"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse nested scopes" $ do
        res <- runMountain $ importer "TestParser.Scopes.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2")]
      it "Should parse ws" $ do
        res <- runMountain $ importer "TestParser.Scopes.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Literal (Thing "1"),Literal (Thing "2"),Literal (Thing "3")]
    describe "Refined" $ do
      it "Should parse refine" $ do
        res <- runMountain $ importer "TestParser.Refines.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Refine (Literal (Thing "1")) (Literal (Thing "2"))
      it "Should parse Either first" $ do
        res <- runMountain $ importer "TestParser.Refines.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Refine (Either [Literal (Thing "1"),Literal (Thing "2")]) (Literal (Thing "3"))
      it "Should parse nested" $ do
        res <- runMountain $ importer "TestParser.Refines.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Refine (Literal (Thing "1")) (Each [Literal (Thing "2"),Literal (Thing "3")])
      it "Should parse ws" $ do
        res <- runMountain $ importer "TestParser.Refines.4_ws"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Refine (Literal (Thing "1")) (Literal (Thing "2")),Refine (Literal (Thing "1")) (Literal (Thing "2"))]
    describe "Unique" $ do
      it "Should parse Unique" $ do
        res <- runMountain $ importer "TestParser.Uniques.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Unique emptyHash (Literal $ Thing "2")
      it "Should parse Either second" $ do
        res <- runMountain $ importer "TestParser.Uniques.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Unique emptyHash (Either [Literal (Thing "2"),Literal (Thing "3")])
      it "Should parse nested" $ do
        res <- runMountain $ importer "TestParser.Uniques.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Unique emptyHash (Unique emptyHash (Literal (Thing "2")))
    describe "Call" $ do
      it "Should parse Call" $ do
        res <- runMountain $ importer "TestParser.Calls.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Call (Reference "a") (Reference "b"),Call (Reference "a") (Reference "b")]
      it "Should parse Either first, call second" $ do
        res <- runMountain $ importer "TestParser.Calls.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Call (Literal (Thing "1")) (Literal (Thing "2")),Literal (Thing "3")]
      it "Should parse nested" $ do
        res <- runMountain $ importer "TestParser.Calls.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Call (Reference "x") (Call (Reference "y") (Reference "z"))
    describe "Has" $ do
      it "Should parse Has" $ do
        res <- runMountain $ importer "TestParser.Hass.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Has (Reference "a") (Reference "b"),Has (Reference "a") (Reference "b")]
      it "Should parse Either first, has second" $ do
        res <- runMountain $ importer "TestParser.Hass.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Has (Literal (Thing "1")) (Literal (Thing "2")),Literal (Thing "3")]
      it "Should parse nested" $ do
        res <- runMountain $ importer "TestParser.Hass.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Has (Each [Reference "x",Reference "y"]) (Reference "z")
    describe "Bind" $ do
      it "Should parse Bind colon and equal" $ do
        res <- runMountain $ importer "TestParser.Bind.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Bind (Reference "a") (Reference "b"),Bind (Reference "a") (Reference "b")]
      it "= should parse last" $ do
        res <- runMountain $ importer "TestParser.Bind.2_="
        let (Right (val, env), log) = res
        val `shouldBe` Bind (Reference "1") (Either [Reference "2",Reference "3"])
      it ": should parse first" $ do
        res <- runMountain $ importer "TestParser.Bind.3_:"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Bind (Reference "1") (Reference "2"),Reference "3"]
      it "mixing = and : should be right" $ do
        res <- runMountain $ importer "TestParser.Bind.4_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [Bind (Bind (Reference "x") (Bind (Reference "y") (Reference "z"))) (Reference "a"),Bind (Reference "x") (Bind (Bind (Reference "y") (Reference "z")) (Reference "a"))]
    describe "Select" $ do
      it "Should parse Select" $ do
        res <- runMountain $ importer "TestParser.Select.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Scope [
          Select (Reference "a") ["b"],
          Select (Tuple [Reference "a",Reference "b"]) ["a"],
          Select (Tuple [Reference "a",Reference "b"]) ["a","b","c","d"]]
      it "Should parse select first, other second" $ do
        res <- runMountain $ importer "TestParser.Select.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Reference "a",Select (Reference "b") ["c"]]
      it "Should parse recursive selects" $ do
        res <- runMountain $ importer "TestParser.Select.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Select (Reference "x") ["y","z"]
    describe "Contexts" $ do
      it "Should parse Contexts" $ do
        res <- runMountain $ importer "TestParser.Contexts.1_basic"
        let (Right (val, env), log) = res
        val `shouldBe` Context (M.fromList [("x",Literal (Thing "1"))]) (Tuple [Reference "x",Literal (Thing "2")])
      it "Should parse either first, other second" $ do
        res <- runMountain $ importer "TestParser.Contexts.2_either"
        let (Right (val, env), log) = res
        val `shouldBe` Either [Reference "a",Context (fromList [("x",Reference "1")]) (Reference "x")]
      it "Should handle recursive contexts" $ do
        res <- runMountain $ importer "TestParser.Contexts.3_recursive"
        let (Right (val, env), log) = res
        val `shouldBe` Context (M.fromList [("x",Reference "1"),("y",Reference "2")]) (Tuple [Reference "x",Reference "y"])
      
      -- it "Should parse recursive selects" $ do
      --   res <- runMountain $ importer "TestParser.Context.3_recursive"
      --   let (Right (val, env), log) = res
      --   val `shouldBe` Select (Reference "x") ["y","z"]
    -- describe "Context" $ do

