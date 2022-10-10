{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module MountainSpec (spec) where

import Data.Either
import qualified Data.Map.Strict as M
import Data.Map.Strict (fromList)
import Debug.Trace

import Test.Hspec
import Mountain
import Hash
import MountainParser

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
        it "Simple recursion should be identified" $ do
          let simple_rec = Bind (Reference "x") (Function [Literal $ Thing "a", Reference "x"])
          isRecursive simple_rec `shouldBe` True
        it "should not remove binding on finished rec" $ do
          let simple_rec = Scope [Bind (Reference "x") (Function [Literal $ Thing "a", Reference "x"])]
          res <- runMountain $ stepMany 10 simple_rec
          let (Right (val, env), log) = res
          val `shouldBe` Bind (Reference "x") (Function [Literal (Thing "a"),Reference "x"])
        it "should step other parts of rec cat" $ do
          let need_eval_rec = Bind (Reference "x") (Function [Call (Function [a,b]) a, Reference "x"])
          res <- runMountain $ stepMany 10 need_eval_rec
          let (Right (val, env), log) = res
          val `shouldBe` Bind (Reference "x") (Function [Literal (Thing "b"),Reference "x"])
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
            print res
            let (Right (val, MountainEnv _ env), log) = res
            let (Scope [x]) = nat_import'
            val `shouldBe` x
      it "Should set unique values" $ do
        let val = Scope [Import (Bind (Reference "n") (Select (Reference "Tests") ["Import","2_unique"])),Reference "n"]
        res <- runMountain $ stepMany 20 val
        print res
        let (Right (val, MountainEnv _ env), log) = res
        let (Unique h x) = val
        h `shouldNotBe` Nil
        x `shouldBe` Literal (Thing "x")
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
        term `shouldBe` Scope [Context (M.fromList [("x",Literal (Thing "1"))]) (Reference "x")]
        res <- runMountain $ stepMany 10 term
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Literal (Thing "1")
      it "should join contexts in eithers" $ do
        let term = fromRight (error "404") $ parseString "(<x:#1> => x) | (<x:#2> => x)"
        term `shouldBe` Scope [Either [Context (fromList [("x",Literal (Thing "1"))]) (Reference "x"),Context (fromList [("x",Literal (Thing "2"))]) (Reference "x")]]
        res <- runMountain $ stepMany 10 term
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, MountainEnv _ env) -> do
            val `shouldBe` Either [Literal (Thing "1"),Literal (Thing "2")]
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
        let parse_str = "\\{ (x,y) = (#a,#b); (x,y)}"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 10 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Literal (Thing "a"),Literal (Thing "b")]
      it "should handle unbound references in tuples" $ do
        let parse_str = "\\{ (x,y) = (k,b); (x,y)}"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 10 $ res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Reference "k", Reference "b"]
      it "should directly bind to recursive functions" $ do
        let parse_str = "\\{ x = #a -> x; x}"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 5 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Bind (Reference "x") (Function [Literal (Thing "a"),Reference "x"])
      it "should handle recursion" $ do
        let parse_str = "#a -> #a -> y = (x = #a -> x); y"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Bind (Reference "x") (Function [Literal (Thing "a"),Reference "x"])
      it "should handle left binds" $ do
        let parse_str = "(x = ?) = #a; x"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Literal (Thing "a")
      it "should bind placeholder functions" $ do
        let parse_str = "z = x -> y = #a -> #b; z"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "b")]
      it "should bind placeholder functions and not their internal scope" $ do
        let parse_str = "z = x -> y = (a:#a -> a); z"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "a")]
      it "Should handle simple contexts on the lhs of bind" $ do
        let parse_str = "(z:a -> z) = #a -> #a; a"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` a
      it "Should keep inner var of lhs of bind unbound" $ do
        let parse_str = "(z:a -> z) = #a -> #a; z"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Reference "z"
      it "Should handle simple contexts on the lhs of bind" $ do
        let parse_str = "(z:a -> z) = #a -> #a; a"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` a
      it "Should handle contexts on both sides" $ do
        let parse_str = "(a:#x -> b:#y -> (a,b)) = (a1:#x -> b1:#y -> (a1,b1)); #a"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` a
        env `shouldBe` []
      it "Should bind references onto itself without doing anything else" $ do
        let parse_str = "a = a; a"
        let res = fromRight (error "404") $ parseString parse_str
        res <- runMountain $ stepMany 20 res
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Reference "a"
        env `shouldBe` []
    describe "has" $ do
      it "(things) equal things don't have each other" $ do
          res <- runMountain $ stepMany 20 (Has a a)
          let (res', log) = res
          res' `shouldBe` Left (BadHas a a )
      it "(Set) set of thing has the thing" $ do
          res <- runMountain $ stepMany 20 (Has (Set [a]) a)
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` Literal (Thing "a")
      it "(Set) set of thing a does not have thing b" $ do
          res <- runMountain $ stepMany 20 (Has (Set [a]) b)
          let (Left e, log) = res
          e `shouldBe` BadBind a b
      it "(Set) should have only its elements" $ do
          res <- runMountain $ stepMany 20 (Has (Set [a,b]) a)
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` a
          env `shouldBe` []

          res <- runMountain $ stepMany 20 (Has (Set [a,b]) b)
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` b
          env `shouldBe` []
      it "(Set) should have eithers" $ do
          res <- runMountain $ stepMany 20 (Has (Set [a,b]) (Either [a,b]))
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` Either [a,b]
          env `shouldBe` []
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
          res <- runMountain $ stepMany 20 $ Has (Tuple [Set [a]]) a
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` a
          env `shouldBe` []
      it "(Tuples) should unwrap values" $ do
          let a' = Tuple [a]
          let env = defaultEnv
          result <- stepAndResult env (Has (Set [a]) a')
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
      it "(Functions) properly checks simple function" $ do
          res <- runMountain $ stepMany 20 (Has (Function [Set [a], Set [b]]) (Function [a, b]))
          let (Right (val, MountainEnv _ env), log) = res
          val `shouldBe` Function [a, b]
          env `shouldBe` []
      it "(references) should supply type var" $ do
        result <- runMountain $ stepMany 20 $ Has (Reference "x") a
        let (Right (val, MountainEnv _ env), log) = result
        val `shouldBe` Literal (Thing "a")
        env `shouldBe` []
      it "(references) should handle simple function" $ do
        res <- runMountain $ dotImportFile "Tests.Has.1_simple_function"
        let (Right (val, env), _) = res
        val `shouldBe` Scope [Has (Function [Reference "a",Reference "a"]) (Function [Literal (Thing "1"),Literal (Thing "2")])]
      it "(references) should handle backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.2_backtrack"
        let (Right (val, env), _) = res
        val `shouldBe` Scope [Has (Function [Reference "a",Reference "a"]) (Function [Reference "x",Literal (Thing "2")])]
        res <- runMountain $ stepMany 20 val
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [Literal (Thing "2"),Literal (Thing "2")]
        env `shouldBe` []
      it "(references) should handle functions and backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.3_function_binding"
        let (Right (val, env), _) = res
        val `shouldBe` Scope [Has (Function [Reference "a",Reference "a"]) (Function [Function [Reference "x",Reference "y"],Literal (Thing "2"),Literal (Thing "3")])]
        res <- runMountain $ stepMany 20 val
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [
          Function [Reference "x",Reference "y"],
          Literal (Thing "2"),
          Literal (Thing "3")]
        env `shouldBe` []
      it "(references) should handle tuples and backtracking" $ do
        res <- runMountain $ dotImportFile "Tests.Has.4_tuple"
        let (Right (val, env), _) = res
        val `shouldBe` Scope [Has (Tuple [Reference "a",Reference "a"]) (Tuple [Reference "x",Literal (Thing "2")])]
        res <- runMountain $ stepMany 10 val
        let (Right (val, MountainEnv _ env), _) = res
        val `shouldBe` Tuple [Literal (Thing "2"),Literal (Thing "2")]
        env `shouldBe` []
      it "(functions) should handle simple functions 1" $ do
        let example = "(a -> a)@(#a -> #a)"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [a,a]
        env `shouldBe` []
      it "(functions) should handle simple functions 2" $ do
        let example = "(a -> a)@(#1 -> #2)"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Left val, log) = res
        val `shouldBe` BadBind (Literal $ Thing "1") (Literal $ Thing "2")
      it "(functions) should handle inner contexts rhs" $ do
        let example = "(a -> a) @ (x:#z -> x)"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [Literal (Thing "z"),Literal (Thing "z")]
        env `shouldBe` []
      it "(functions) should handle inner contexts lhs" $ do
        let example = "(z:a -> z) @ (#z -> #z)"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [Literal (Thing "z"),Literal (Thing "z")]
        env `shouldBe` []
      it "(functions) should handle inner contexts both lhs rhs" $ do
        let example = "(z:a -> z) @ (x:#r -> x)"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Function [Literal $ Thing "r", Literal $ Thing "r"]
        env `shouldBe` []
      it "(References) should leave unbindables alone" $ do
        let example = "(a,b) = x"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Reference "a",Reference "b"]
        env `shouldBe` []
    describe "Select" $ do
      it "should select first elem from a tuple" $ do
        let example = "(x:#a, y:#b).x"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` a
        env `shouldBe` []
      it "should select second element of a tuple" $ do
        let example = "(x:#a, y:#b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Literal (Thing "b")
        env `shouldBe` []
      it "should return the original tuple if both selected" $ do
        let example = "(x:#a, y:#b).[x,y]"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Tuple [Bind (Reference "x") (Literal (Thing "a")),Bind (Reference "y") (Literal (Thing "b"))]
        env `shouldBe` []
      it "should throw error on bad selection" $ do
        let example = "(x:#a, y:#b).[z]"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Left e, log) = res
        e `shouldBe` BadSelect unit ["z"]
      it "should select on sets" $ do
        let example = "{x:#a, y:#b}.y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` b
        env `shouldBe` []
      it "should select on eithers" $ do
        let example = "(x:#a | y:#b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` b
        env `shouldBe` []
      it "should handle dependent tuples" $ do
        let example = "(x:#a, y:x).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` a
        env `shouldBe` []
      it "should handle structure binds of refs" $ do
        let example = "((x,y):a, y:x).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, MountainEnv _ env), log) = res
        val `shouldBe` Reference "x"
        env `shouldBe` []
      it "should throw error on functions" $ do
        let example = "(y:a -> b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Left e, log) = res
        e `shouldBe` BadSelect (Function [Bind (Reference "y") (Reference "a"),Reference "b"]) ["y"]
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