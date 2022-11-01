{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module SimpleMountain.SimpleMountainParserSpec (spec) where

import SimpleMountain.SimpleMountain
import SimpleMountain.SimpleMountainParser
import Hash
import Data.Map.Strict as M
import Data.Either
import qualified Data.UUID as UUID
import Test.Hspec

-- TODO Split into respective files

spec :: Spec
spec = parallel $ do
    describe "Structure" $ do
      describe "Var" $ do
        it "Should parse Var" $ do
          let res = parseString "somevar"
          res `shouldBe` Right (Var "somevar")
        it "Should parse underscore var" $ do
          let res = parseString "some_var"
          res `shouldBe` Right (Var "some_var")
        it "Should parse quoted var" $ do
          let res = parseString "some_var'"
          res `shouldBe` Right (Var "some_var'")
      describe "Function" $ do
        it "Should parse simple function" $ do
          let res = parseString "a -> b"
          res `shouldBe` Right (Function (Var "a") (Var "b"))
        it "Should parse long chain function" $ do
          let res = parseString "a -> b -> c -> d -> e"
          res `shouldBe` Right (Function (Var "a") (Function (Var "b") (Function (Var "c") (Function (Var "d") (Var "e")))))
        it "Should parse close function" $ do
          let res = parseString "a->b"
          res `shouldBe` Right (Function (Var "a") (Var "b"))
      describe "Tuple" $ do
        it "should parse a parenthesized var" $ do
          let res = parseString "(b)"
          res `shouldBe` Right (Var "b")
        it "should parse a parenthesized function" $ do
          let res = parseString "(a -> b)"
          res `shouldBe` Right (Function (Var "a") (Var "b"))
        it "should parse a lefty function" $ do
          let res = parseString "(a -> b) -> c"
          res `shouldBe` Right (Function (Function (Var "a") (Var "b")) (Var "c"))
      describe "Extern" $ do
        it "Should parse is" $ do
          let res = parseString "is"
          res `shouldBe` Right (Extern Is)
        it "Should parse assert" $ do
          let res = parseString "assert"
          res `shouldBe` Right (Extern Assert)
        it "Should parse assertFail" $ do
          let res = parseString "assertFail"
          res `shouldBe` Right (Extern AssertFail)
      describe "Call" $ do
        it "Should parse a spaced call" $ do
          let res = parseString "a b"
          res `shouldBe` Right (Call (Var "a") (Var "b"))
        it "Should parse a close call" $ do
          let res = parseString "a(b)"
          res `shouldBe` Right (Call (Var "a") (Var "b"))
        it "Should parse a inverted call" $ do
          let res = parseString "a`b`"
          res `shouldBe` Right (Call (Var "b") (Var "a"))
      describe "Let" $ do
        it "Should parse a let" $ do
          let res = parseString "a = b; a"
          res `shouldBe` Right (Let "a" (Var "b") (Var "a"))
        it "Should parse a let in a function" $ do
          let res = parseString "x -> a = b; a"
          res `shouldBe` Right (Function (Var "x") (Let "a" (Var "b") (Var "a")))
      describe "Context" $ do
        it "Should parse a context" $ do
          let res = parseString "<a=b> => a"
          res `shouldBe` Right (Context (fromList [("a",Var "b")]) (Var "a"))
        it "Should parse a multiple context" $ do
          let res = parseString "<a=b;c=d> => a"
          res `shouldBe` Right (Context (fromList [("a",Var "b"), ("c", Var "d")]) (Var "a"))
