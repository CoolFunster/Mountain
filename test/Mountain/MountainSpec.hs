{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Mountain.MountainSpec (spec) where

import Data.Either
import qualified Data.Map.Strict as M
import Debug.Trace

import Test.Hspec
import Mountain.Mountain
import Mountain.MountainParser

-- TODO Split into respective files

spec :: Spec
spec = do
    let stepAndResult env x = fst <$> runMountainContextT env (step x)
    let thing  = Literal $ Thing "thing"
    let thing2 = Literal $ Thing "thing2"
    let thing3 = Literal $ Thing "thing3"
    let a = Literal $ Thing "a"
    let b = Literal $ Thing "b"
    let c = Literal $ Thing "c"
    let d = Literal $ Thing "d"
    let a_b = Function [a, b]
    let b_c = Function [b, c]
    let a_c = Function [a, c]
    let a_d = Function [a, d]
    describe "Recursive" $ do
        let simple_rec = fromRight (error "bad simple_rec parsing") $ parseString "Self = ( Self | ((#a -> #b)#a, Self))"
        let nat = fromRight (error "bad nat parsing") $ parseString "Nat = {#Z | (#S, Nat)}"
        it "Simple recursion should be identified" $ do
          isRecursive simple_rec `shouldBe` True
        it "Nats are recursive" $ do
          isRecursive nat `shouldBe` True
        it "should not step recursive nats" $ do
          res <- runMountain $ stepMany 10 nat
          let (Right (val, env), log) = res
          val `shouldBe` nat
        it "should step other parts of rec cat" $ do
          res <- runMountain $ stepMany 10 simple_rec
          let (Right (val, env), log) = res
          val `shouldBe` Bind (Reference "Self") (Either [Reference "Self",Tuple [Literal (Thing "b"),Reference "Self"]])
    describe "Import" $ do
      it "Should import nats" $ do
        res <- runMountain $ dotImportFile "Tests.Import.1_import_nat"
        nat_import <- runMountain $ dotImportFile "Tests.Import.1_test_nat"
        let (res', log) = res
        let (Right (nat_import', _), log) = nat_import
        case res' of
          Left e -> error (show e)
          Right (val, env) -> do
            val `shouldBe` Scope [Import (Bind (Reference "Nat") (Select (Reference "Tests") ["Import","1_test_nat"])),Reference "Nat"]
            res <- runMountain $ stepMany 20 val
            let (Right (val, MountainEnv _ env), log) = res
            val `shouldBe` nat_import'
            map M.toList env `shouldBe` [[("Nat",nat_import')]]
    describe "Step" $ do
      it "should keep sets in a context" $ do
        let nat = fromRight (error "bad nat parsing") $ parseString "Nat = {zero:#Z | succ:(#S, Nat)}"
        res <- runMountain $ stepMany 10 nat
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Bind (Reference "Nat") (Set [Either [Bind (Reference "zero") (Literal (Thing "Z")),Bind (Reference "succ") (Tuple [Literal (Thing "S"),Reference "Nat"])]])
      it "should resolve contexts" $ do
        let term = fromRight (error "404") $ parseString "<x:#1> => x"
        term `shouldBe` Context (M.fromList [("x",Literal (Thing "1"))]) (Reference "x")
        res <- runMountain $ stepMany 10 term
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Literal (Thing "1")
    describe "Bind" $ do
      it "should bind things and create a scope" $ do
        res <- runMountain $ bind (Reference "x") a
        let (Right (val, env), _) = res
        val `shouldBe` Literal (Thing "a")
        toList env `shouldBe` [[("x", a)]]
      it "should bind identical things" $ do
        res <- runMountain $ bind a a
        let (Right (val, env), _) = res
        val `shouldBe` a
      it "should handle dependent tuples" $ do
        let bindable = Tuple [Reference "x", Reference "y"]
        let bindee = Tuple [a, b]
        res <- runMountain $ stepBindMany 5 $ Bind bindable bindee
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Literal (Thing "a"),Literal (Thing "b")]
        map M.toList env `shouldBe` [[("x", a), ("y", b)]]
      it "should handle unbound references in tuples" $ do
        let bindable = Tuple [Reference "x", Reference "y"]
        let bindee = Tuple [Reference "k", Reference "b"]
        res <- runMountain $ stepBindMany 5 $ Bind bindable bindee
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Reference "k", Reference "b"]
        map M.toList env `shouldBe` [[("x", Reference "k"), ("y", Reference "b")]]
      it "should directly bind to recursive functions" $ do
        let bindable = Reference "x"
        let bindee = Bind (Reference "Self") $ Function [a, Reference "Self"]
        res <- runMountain $ stepBind $ Bind bindable bindee
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` bindee
        map M.toList env `shouldBe` [[("x", bindee)]]
      it "should handle recursion" $ do
        let bindable = Function [a, a, Reference "y"]
        let bindee = Bind (Reference "Self") $ Function [a, Reference "Self"]
        res <- runMountain $ stepBindMany 20 $ Bind bindable bindee
        print res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [a, a, bindee]
        map M.toList env `shouldBe` [[("y", bindee)]]
      it "should handle left binds" $ do
        let bindable = Bind (Reference "x") Wildcard
        let bindee = Literal (Thing "a")
        res <- runMountain $ bind bindable bindee
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Bind (Reference "x") (Bind Wildcard (Literal (Thing "a")))
        map M.toList env `shouldBe` []
        res <- runMountain $ stepBind val
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Bind (Reference "x") (Literal (Thing "a"))
        map M.toList env `shouldBe` []
        res <- runMountain $ stepBind val
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Literal (Thing "a")
        map M.toList env `shouldBe` [[("x", Literal $ Thing "a")]]
      it "should bind placeholder functions" $ do
        let bindable = Function [Reference "x", Reference "y"]
        let bindee = Function [a, b]
        res <- runMountain $ stepBindMany 5 (Bind bindable bindee)
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "b")]
        map M.toList env `shouldBe` [[("x", Literal $ Thing "a"),("y", Literal $ Thing "b")]]
      it "should bind placeholder functions and not their internal scope" $ do
        let bindable = Function [Reference "x", Reference "y"]
        let bindee = Function [Bind (Reference "z") a, Reference "z"]
        res <- runMountain $ stepBindMany 10 (Bind bindable bindee)
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "a")]
        map M.toList env `shouldBe` [[("x", a), ("y", a)]]
    describe "has" $ do
      it "(things) equal things don't have each other" $ do
          let env = defaultEnv
          result <- stepAndResult env (Has a a)
          result `shouldBe` Left (BadHas a a )
      it "(Set) set of thing has the thing" $ do
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a]) a)
          let Right (val, env) = result
          val `shouldBe` Literal (Thing "a")
      it "(Set) set of thing a does not have thing b" $ do
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a]) b)
          result `shouldBe` Left (BadHas (Set [a]) b)
      it "(Set) should have only its elements" $ do
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a,b]) b)
          let Right (val, env) = result
          val `shouldBe` Either [Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` b
          toList env `shouldBe` []
      it "(Set) should have eithers" $ do
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a,b]) (Either [a,b]))
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [Literal (Thing "a"),Literal (Thing "b")]
          toList env `shouldBe` []
      it "(Set,Either) Eithers of Sets should distribute" $ do
          let env = defaultEnv
          let val = Has (Either [Set [a], Set [b]]) b
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
      it "(Set,Each) Eaches of Sets should distribute" $ do
          let env = defaultEnv
          let val = Has (Each [Set [a], Set [b]]) b
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Each [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
      it "(Tuples) should unwrap types" $ do
          let a' = Tuple [Set [a]]
          let env = defaultEnv
          result <- stepAndResult env (Has a' a)
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Literal (Thing "a")
          toList env `shouldBe` []
      it "(Tuples) should unwrap values" $ do
          let a' = Tuple [a]
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a]) a')
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
      it "(Functions) properly checks simple function" $ do
          let ta_b = Function [Set [a], Set [b]]
          let a_b = Function [a, b]
          let env = defaultEnv
          result <- stepAndResult env (Has ta_b a_b)
          let Right (val, env) = result
          val `shouldBe` Function [Has (Set [Literal (Thing "a")]) (Literal (Thing "a")),Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
      it "(references) should supply type var" $ do
        let env = defaultEnv
        result <- stepAndResult env (Has (Reference "x") a)
        let Right (val, e) = result
        val `shouldBe` Literal (Thing "a")
        map M.toList (environment e) `shouldBe` [[("x",Set [a])]]
      it "(references) should handle simple function" $ do
        res <- runMountain $ dotImportFile "Tests.Has.1_simple_function"
        let (Right (val, env), _) = res
        val `shouldBe` Has (Function [Reference "a",Reference "a"]) (Function [Literal (Thing "1"),Literal (Thing "2")])
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Has (Reference "a") (Literal (Thing "1")),Has (Reference "a") (Literal (Thing "2"))]
        map M.toList (environment env) `shouldBe` []
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Literal (Thing "1"),Has (Reference "a") (Literal (Thing "2"))]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "1")])]]
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Literal (Thing "1"),Has (Set [Literal (Thing "1")]) (Literal (Thing "2"))]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "1")])]]
        result <- stepAndResult env val
        result `shouldBe` Left (BadHas (Set [Literal (Thing "1")]) (Literal (Thing "2")))
      it "(references) should handle backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.2_backtrack"
        let (Right (val, env), _) = res
        val `shouldBe` Has (Function [Reference "a",Reference "a"]) (Function [Reference "x",Literal (Thing "2")])
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Has (Reference "a") (Reference "x"),Has (Reference "a") (Literal (Thing "2"))]
        map M.toList (environment env) `shouldBe` []
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Has (Reference "a") (Reference "x"),Literal (Thing "2")]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "2")])]]
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Has (Set [Literal (Thing "2")]) (Reference "x"),Literal (Thing "2")]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "2")])]]
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Literal (Thing "2"),Literal (Thing "2")]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "2")]),("x",Literal (Thing "2"))]]
      it "(references) should handle functions and backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.3_function_binding"
        let (Right (val, env), _) = res
        val `shouldBe` Has (Function [Reference "a",Reference "a"]) (Function [Function [Reference "x",Reference "y"],Literal (Thing "2"),Literal (Thing "3")])
        res <- runMountain $ stepMany 10 val
        let (Right (val, env), _) = res
        val `shouldBe` Function [
          Function [Reference "x",Reference "y"],
          Literal (Thing "2"),
          Literal (Thing "3")]
        map M.toList (environment env) `shouldBe` [[("a",Set [Function [Reference "x",Reference "y"]])]]
      it "(references) should handle tuples and backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.4_tuple"
        let (Right (val, env), _) = res
        val `shouldBe` Has (Tuple [Reference "a",Reference "a"]) (Tuple [Reference "x",Literal (Thing "2")])
        res <- runMountain $ stepMany 10 val
        let (Right (val, env), _) = res
        val `shouldBe` Tuple [Literal (Thing "2"),Literal (Thing "2")]
        map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "2")]),("x",Literal (Thing "2"))]]
    -- describe "Select" $ do
      -- it "Should "
    -- describe "Base.Data.Numeric.Natural" $ do
    --   it "should import and select correctly" $ do
    --     res <- runMountain $ dotImportFile "Tests.Base.Data.Numeric.testNatural"
    --     let (Right (val, env), _) = res
    --     val `shouldBe` Scope [Import (Bind (Reference "nat") (Select (Reference "Base") ["Data","Numeric","Natural"])),Select (Reference "nat") ["increment"]]
    --     res <- runMountain $ stepMany 10 val
    --     print res
    --     let (Right (val, env), _) = res
    --     val `shouldBe` Tuple [Literal (Thing "2"),Literal (Thing "2")]
    --     map M.toList (environment env) `shouldBe` [[("a",Set [Literal (Thing "2")]),("x",Literal (Thing "2"))]]