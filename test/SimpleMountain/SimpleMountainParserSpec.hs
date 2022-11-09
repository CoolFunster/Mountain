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
spec = do
    describe "Structure" $ do
      describe "Var" $ do
        it "Should parse Var" $ do
          let res = parseString "somevar"
          res `shouldBe` Right (Term $ Ref "somevar")
        it "Should parse underscore var" $ do
          let res = parseString "some_var"
          res `shouldBe` Right (Term $ Ref "some_var")
        it "Should parse quoted var" $ do
          let res = parseString "some_var'"
          res `shouldBe` Right (Term $ Ref "some_var'")
      describe "Function" $ do
        it "Should parse simple function" $ do
          let res = parseString "a -> b"
          res `shouldBe` Right (Term $ Function (Ref "a") (Ref "b"))
        it "Should parse long chain function" $ do
          let res = parseString "a -> b -> c -> d -> e"
          res `shouldBe` Right (Term $ Function (Ref "a") (Function (Ref "b") (Function (Ref "c") (Function (Ref "d") (Ref "e")))))
        it "Should parse close function" $ do
          let res = parseString "a->b"
          res `shouldBe` Right (Term $ Function (Ref "a") (Ref "b"))
      describe "Literal" $ do
        it "Should parse is" $ do
          let res = parseString "is"
          res `shouldBe` Right (Term $ Literal Is)
        it "Should parse assert" $ do
          let res = parseString "assert"
          res `shouldBe` Right (Term $ Literal Assert)
        it "Should parse assertFail" $ do
          let res = parseString "assertFail"
          res `shouldBe` Right (Term $ Literal AssertFail)
      describe "Call" $ do
        it "Should parse a spaced call" $ do
          let res = parseString "a b"
          res `shouldBe` Right (Term $ Call (Ref "a") (Ref "b"))
        it "Should parse a close call" $ do
          let res = parseString "a(b)"
          res `shouldBe` Right (Term $ Call (Ref "a") (Ref "b"))
        it "Should parse a inverted call" $ do
          let res = parseString "a`b`"
          res `shouldBe` Right (Term $ Call (Ref "b") (Ref "a"))
      describe "Let" $ do
        it "Should parse a let" $ do
          let res = parseString "a = b; a"
          res `shouldBe` Right (Term $ Let "a" (Ref "b") (Ref "a"))
        it "Should parse a let in a function" $ do
          let res = parseString "x -> a = b; a"
          res `shouldBe` Right (Term $ Function (Ref "x") (Let "a" (Ref "b") (Ref "a")))
      describe "Context" $ do
        it "Should parse a context" $ do
          let res = parseString "<a=b> => a"
          res `shouldBe` Right (Term $ Context (fromList [("a",Ref "b")]) (Ref "a"))
        it "Should parse a multiple context" $ do
          let res = parseString "<a=b;c=d> => a"
          res `shouldBe` Right (Term $ Context (fromList [("a",Ref "b"), ("c", Ref "d")]) (Ref "a"))
