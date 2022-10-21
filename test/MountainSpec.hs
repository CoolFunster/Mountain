{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module MountainSpec (spec) where

import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
      it "should not remove binding on finished rec" $ do
        let simple_rec = Scope [Recursive "x" (Function [Literal $ Thing "a", Reference "x"])]
        res <- runMountain $ stepMany 10 simple_rec
        let (Right (val, env), log) = res
        val `shouldBe` Recursive "x" (Function [Literal (Thing "a"),Reference "x"])
      it "should step other parts of rec cat" $ do
        let need_eval_rec = Recursive "x" (Function [Call (Function [a,b]) a, Reference "x"])
        res <- runMountain $ stepMany 10 need_eval_rec
        let (Right (val, env), log) = res
        val `shouldBe` Recursive "x" (Function [Literal (Thing "b"),Reference "x"])
    describe "Import" $ do
      it "Should import nats" $ do
        res <- runMountain $ dotImportFile "Tests.Import.1_import_nat"
        let val = Import (Reference "Tests.Import.1_import_nat")
        res <- runMountain $ stepMany 20 val
        -- print res
        let (Right (val, env), log) = res
        val `shouldBe` Recursive "Nat" (Set [Either [Def (Reference "zero") (Literal (Thing "Z")),Def (Reference "succ") (Tuple [Literal (Thing "S"),Reference "Nat"])]])
      it "Should set unique values" $ do
        let val = Scope [Import (Def (UniqueRef (Reference "n")) (Select (Reference "Tests") ["Import","2_unique"])),Reference "n"]
        res <- runMountain $ stepMany 20 val
        let (Right (val, env), log) = res
        val `shouldBe` UniqueRef (Literal (Thing "x"))
    describe "Step" $ do
      it "should keep sets in a context" $ do
        let nat = fromRight (error "bad nat parsing") $ parseString "Nat ~ {zero:#Z | succ:(#S, Nat)}"
        res <- runMountain $ stepMany 10 nat
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Recursive "Nat" (Set [Either [Def (Reference "zero") (Literal (Thing "Z")),Def (Reference "succ") (Tuple [Literal (Thing "S"),Reference "Nat"])]])
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
        let term = normalize $ fromRight (error "404") $ parseString "(<x:#1> => x) | (<x:#2> => x)"
        term `shouldBe` Scope [Either [Context (fromList [("x",Literal (Thing "1"))]) (Reference "x"),Context (fromList [("x",Literal (Thing "2"))]) (Reference "x")]]
        res <- runMountain $ stepMany 10 term
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Either [Literal (Thing "1"),Literal (Thing "2")]
      it "shouldn't ordinary unique refs" $ do
        res <- runMountain $ stepMany 10 (Scope [Function [UniqueRef (Reference "x"),UniqueRef (Reference "x")]])
        let (res', log) = res
        case res' of
          Left e -> error (show e ++ "\n\n" ++ show log)
          Right (val, env) -> do
            val `shouldBe` Function [UniqueRef (Reference "x"),UniqueRef (Reference "x")]
    -- describe "has" $ do
    --   it "(things) equal things don't have each other" $ do
    --       res <- runMountain $ stepMany 20 (Has a a)
    --       let (res', log) = res
    --       res' `shouldBe` Left (BadHas a a )
    --   it "(Set) set of thing has the thing" $ do
    --       res <- runMountain $ stepMany 20 (Has (Set [a]) a)
    --       let (Right (val, env), log) = res
    --       val `shouldBe` Literal (Thing "a")
    --   it "(Set) set of thing a does not have thing b" $ do
    --       res <- runMountain $ stepMany 20 (Has (Set [a]) b)
    --       let (Left e, log) = res
    --       e `shouldBe` BadDef a b
    --   it "(Set) should have only its elements" $ do
    --       res <- runMountain $ stepMany 20 (Has (Set [a,b]) a)
    --       let (Right (val, env), log) = res
    --       val `shouldBe` a
    --       toList env `shouldBe` toList defaultEnv

    --       res <- runMountain $ stepMany 20 (Has (Set [a,b]) b)
    --       let (Right (val, env), log) = res
    --       val `shouldBe` b
    --       toList env `shouldBe` toList defaultEnv
    --   it "(Set) should have eithers" $ do
    --       res <- runMountain $ stepMany 20 (Has (Set [a,b]) (Either [a,b]))
    --       let (Right (val, env), log) = res
    --       val `shouldBe` Either [a,b]
    --       toList env `shouldBe` toList defaultEnv
    --   it "(Set,Either) Eithers of Sets should distribute" $ do
    --       let env = defaultEnv
    --       let val = Has (Either [Set [a], Set [b]]) b
    --       result <- stepAndResult env val
    --       let Right (val, env) = result
    --       val `shouldBe` Either [
    --         Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
    --         Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
    --       toList env `shouldBe` toList defaultEnv
    --   it "(Set,Each) Eaches of Sets should distribute" $ do
    --       let env = defaultEnv
    --       let val = Has (Each [Set [a], Set [b]]) b
    --       result <- stepAndResult env val
    --       let Right (val, env) = result
    --       val `shouldBe` Each [
    --         Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
    --         Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
    --       toList env `shouldBe` toList defaultEnv
    --   it "(Tuples) should unwrap types" $ do
    --       res <- runMountain $ stepMany 20 $ Has (Tuple [Set [a]]) a
    --       let (Right (val, env), log) = res
    --       val `shouldBe` a
    --       toList env `shouldBe` toList defaultEnv
    --   it "(Tuples) should unwrap values" $ do
    --       let a' = Tuple [a]
    --       let env = defaultEnv
    --       result <- stepAndResult env (Has (Set [a]) a')
    --       let Right (val, env) = result
    --       val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
    --   it "(Functions) properly checks simple function" $ do
    --       res <- runMountain $ stepMany 20 (Has (Function [Set [a], Set [b]]) (Function [a, b]))
    --       let (Right (val, env), log) = res
    --       val `shouldBe` Function [a, b]
    --       toList env `shouldBe` toList defaultEnv
    --   it "(references) should supply type var" $ do
    --     result <- runMountain $ stepMany 20 $ Has (Reference "x") a
    --     let (Right (val, env), log) = result
    --     val `shouldBe` Literal (Thing "a")
    --     toList env `shouldBe` toList defaultEnv
    --   it "(functions) should handle simple functions 1" $ do
    --     let example = "(a -> a)@(#a -> #a)"
    --     let Right term = parseString example
    --     res <- runMountain $ stepMany 20 term
    --     let (Right (val, env), log) = res
    --     val `shouldBe` Function [a,a]
    --     toList env `shouldBe` toList defaultEnv
    --   it "(functions) should handle simple functions 2" $ do
    --     let example = "(a -> a)@(#1 -> #2)"
    --     let Right term = parseString example
    --     res <- runMountain $ stepMany 20 term
    --     let (Left val, log) = res
    --     val `shouldBe` BadDef (Literal $ Thing "1") (Literal $ Thing "2")
    --   it "(functions) should handle inner contexts rhs" $ do
    --     let example = "(a -> a) @ (x:#z -> x)"
    --     let Right term = parseString example
    --     res <- runMountain $ stepMany 20 term
    --     let (Right (val, env), log) = res
    --     val `shouldBe` Function [Def (Reference "x") (Literal (Thing "z")),Literal (Thing "z")]
    --     toList env `shouldBe` toList defaultEnv
    --   it "(functions) should handle inner contexts lhs" $ do
    --     let example = "(z:a -> z) @ (#z -> #z)"
    --     let Right term = parseString example
    --     res <- runMountain $ stepMany 20 term
    --     let (Right (val, env), log) = res
    --     val `shouldBe` Function [Literal (Thing "z"),Literal (Thing "z")]
    --     toList env `shouldBe` toList defaultEnv
    --   it "(functions) should handle inner contexts both lhs rhs" $ do
    --     let example = "(z:a -> z) @ (x:#r -> x)"
    --     let Right term = parseString example
    --     res <- runMountain $ stepMany 20 term
    --     let (Right (val, env), log) = res
    --     val `shouldBe` Function [Def (Reference "x") (Literal (Thing "r")),Literal (Thing "r")]
    --     toList env `shouldBe` toList defaultEnv
    describe "Select" $ do
      it "should select first elem from a tuple" $ do
        let example = "(x:#a, y:#b).x"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, env), log) = res
        val `shouldBe` a
        toList env `shouldBe` toList defaultEnv
      it "should select second element of a tuple" $ do
        let example = "(x:#a, y:#b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        print res
        let (Right (val, env), log) = res
        val `shouldBe` Literal (Thing "b")
        toList env `shouldBe` toList defaultEnv
      it "should return the original tuple if both selected" $ do
        let example = "(x:#a, y:#b).[x,y]"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, env), log) = res
        print log
        val `shouldBe` Tuple [Def (Reference "x") (Literal (Thing "a")),Def (Reference "y") (Literal (Thing "b"))]
        toList env `shouldBe` toList defaultEnv
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
        let (Right (val, env), log) = res
        val `shouldBe` b
        toList env `shouldBe` toList defaultEnv
      it "should select on eithers" $ do
        let example = "(x:#a | y:#b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, env), log) = res
        val `shouldBe` b
        toList env `shouldBe` toList defaultEnv
      it "should handle dependent tuples" $ do
        let example = "(x:#a, y:x).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, env), log) = res
        val `shouldBe` a
        toList env `shouldBe` toList defaultEnv
      it "should handle structure binds of refs" $ do
        let example = "((x,y):(#a, #b), y:#x).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Right (val, env), log) = res
        val `shouldBe` Literal (Thing "x")
        toList env `shouldBe` toList defaultEnv
      it "should throw error on functions" $ do
        let example = "(y:#a -> #b).y"
        let Right term = parseString example
        res <- runMountain $ stepMany 20 term
        let (Left e, log) = res
        e `shouldBe` BadSelect (Function [Def (Reference "y") (Literal (Thing "a")),Literal (Thing "b")]) ["y"]
    describe "Cleanup" $ do
      it "should handle contexts" $ do
        let example = "<x:#2;y:#3> => y"
        let Right term = parseString example
        let res = cleanup term
        res `shouldBe` Scope [Context (fromList [("y",Literal (Thing "3"))]) (Reference "y")]
    describe "Uniquify" $ do
      it "should handle tuples" $ do
        hash <- randHash
        let res = Tuple [Unique hash (Literal $ Thing "x"), Literal $ Thing "y"]
        let res' = cleanup res
        res' `shouldBe` UniqueRef (Tuple [Unique hash (Literal (Thing "x")),Literal (Thing "y")])
      it "should handle this case" $ do
        hash <- randHash
        let res = Call (Scope [Function [Tuple [UniqueRef (Reference "c"),Wildcard],Call (Reference "print") (Tuple [Reference "c",Literal (String "Hello World")])]]) (Unique hash (Tuple [Unique hash (Literal (Thing "Console")),Tuple []]))
        let res' = cleanup res
        res' `shouldBe` Call (Scope [Function [UniqueRef (Tuple [UniqueRef (Reference "c"),Wildcard]),Call (Reference "print") (Tuple [Reference "c",Literal (String "Hello World")])]]) (Unique hash (Tuple [Unique hash (Literal (Thing "Console")),Tuple []]))
    describe "UniquifyRefs" $ do
      it "should handle tuples" $ do
        let parse_str = "(*x,y)"
        let res = fromRight (error "404") $ parseString parse_str
        uniquify res `shouldBe` Scope [UniqueRef (Tuple [UniqueRef (Reference "x"),Reference "y"])]
      it "should handle tuples2" $ do
        let parse_str = "(*3,4)"
        let res = fromRight (error "404") $ parseString parse_str
        let res' = uniquify res
        res' `shouldBe` Scope [UniqueRef (Tuple [UniqueRef (Literal (Int 3)),Literal (Int 4)])]
      it "should handle functions" $ do
        let parse_str = "(*x,y) -> x"
        let res = fromRight (error "404") $ parseString parse_str
        uniquify res `shouldBe` Scope [Function [UniqueRef (Tuple [UniqueRef (Reference "x"),Reference "y"]),Reference "x"]]
      it "should handle this case" $ do
        let parse_str = "(*c, ?) -> print (c, \"Hello World\")"
        let res = fromRight (error "404") $ parseString parse_str
        uniquify res `shouldBe` Scope [Function [UniqueRef (Tuple [UniqueRef (Reference "c"),Wildcard]),Call (Reference "print") (Tuple [Reference "c",Literal (String "Hello World")])]]
      it "should handle this case 2" $ do
        let parse_str = "*(*x,?) = (*3, 4)"
        let res = fromRight (error "404") $ parseString parse_str
        uniquify res `shouldBe` Scope [Def (UniqueRef (Tuple [UniqueRef (Reference "x"),Wildcard])) (UniqueRef (Tuple [UniqueRef (Literal (Int 3)),Literal (Int 4)]))]
    describe "FreeReferences" $ do
      it "should handle contexts" $ do
        let parse_str = "<x:#1;y:#2> => (x, #2)"
        let res = fromRight (error "404") $ parseString parse_str
        freeReferences res `shouldBe` S.empty
      it "should handle contexts 2" $ do
        let parse_str = "<x:#1;y:z> => (x, #2)"
        let res = fromRight (error "404") $ parseString parse_str
        freeReferences res `shouldBe` S.singleton "z"
      it "should handle contexts 3" $ do
        let parse_str = "<x:#1;y:z> => (a, #2)"
        let res = fromRight (error "404") $ parseString parse_str
        freeReferences res `shouldBe` S.fromList ["z", "a"]
      it "should handle contexts 4" $ do
        let parse_str = "<x:#1;y:#2> => (a, #2)"
        let res = fromRight (error "404") $ parseString parse_str
        freeReferences res `shouldBe` S.fromList ["a"]
    describe "Tests" $ do
      describe "Define" $ do
        it "should directly define to recursive functions" $ do
          let parse_str = "\\{ z = x ~ #a -> x; z}"
          let res = fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), log) = res
          val `shouldBe`  Recursive "x" (Function [Literal (Thing "a"),Reference "x"])
        it "should only define structurally" $ do
          let parse_str = "#a -> #a -> y = (x ~ #a -> x); y"
          let res = fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Left e, log) = res
          e `shouldBe` BadDef (Context M.empty (Function [Literal (Thing "a"),Literal (Thing "a"),Reference "y"])) (Recursive "x" (Function [Literal (Thing "a"),Reference "x"]))
        it "should handle left binds" $ do
          let parse_str = "(x = ?) = #a; x"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), _) = res
          val `shouldBe` Literal (Thing "a")
        it "should define placeholder functions" $ do
          let parse_str = "z = x -> y = #a -> #b; z"
          let res = fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), _) = res
          val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "b")]
        it "should define placeholder functions and not their internal scope" $ do
          let parse_str = "z = x -> y = (a:#a -> a); z"
          let res = fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), _) = res
          val `shouldBe` Function [Def (Reference "a") (Literal (Thing "a")),Reference "a"]
        it "Should handle simple contexts on the lhs of bind" $ do
          let parse_str = "(z:a -> z) = #a -> #a; a"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), log) = res
          val `shouldBe` a
        it "Should handle simple contexts on the lhs of bind 2" $ do
          let parse_str = "(z:a -> z) = #a -> #b; a"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Left e, log) = res
          e `shouldBe` BadDef (Literal (Thing "a")) (Literal (Thing "b"))
        it "Should keep inner var of lhs of define unbound" $ do
          let parse_str = "(z:a -> z) = #a -> #a; z"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Left e, log) = res
          e `shouldBe` UnboundId "z"
        it "Should handle simple contexts on the lhs of bind" $ do
          let parse_str = "(z:a -> z) = #a -> #a; a"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), log) = res
          val `shouldBe` a
        it "Should handle contexts on rhs of bind sides" $ do
          let parse_str = "(#x -> #x) = (a1:#x -> a1)"
          let res = normalize $ fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Right (val, env), log) = res
          val `shouldBe` Function [Def (Reference "a1") (Literal (Thing "x")),Reference "a1"]
          toList env `shouldBe` toList defaultEnv
        it "Should throw error on unbound def" $ do
          let parse_str = "a = a; a"
          let res = fromRight (error "404") $ parseString parse_str
          res <- runMountain $ stepMany 20 res
          let (Left e, log) = res
          e `shouldBe` UnboundId "a"