{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module CategoryHashSpec (spec) where

import Test.Hspec
import Category
import CategoryHash
import FrontEnds.AST.V1.CategoryParser

import Data.Either




spec :: Spec
spec = do
    describe "Hash" $ do
      describe "Things" $ do
        it "should hash things same if they are the same" $ do
          let a = getResultOf $ hash (Thing (Name "x"))
          let b = getResultOf $ hash (Thing (Name "x"))
          a `shouldBe` b
        it "should hash things same if they are parenthesized" $ do
          let a = getResultOf $ hash $ Composite Tuple [Thing (Name "x")]
          let b = getResultOf $ hash $ Thing (Name "x")
          a `shouldBe` b
      describe "Sets" $ do
        it "should hash sets the same regardless of order of elems" $ do
          let a = Thing (Name "a")
          let b = Thing (Name "b")
          let set_1 = getResultOf $ hash (Set [a, b])
          let set_2 = getResultOf $ hash (Set [b, a])
          set_1 `shouldBe` set_2
        it "should hash sets of diff elems diff" $ do
          let a = Thing (Name "a")
          let b = Thing (Name "b")
          let set_1 = getResultOf $ hash (Set [a])
          let set_2 = getResultOf $ hash (Set [b])
          set_1 `shouldNotBe` set_2
        it "should hash sets of same elems diff numbers diff" $ do
          let a = Thing (Name "a")
          let b = Thing (Name "b")
          let set_1 = getResultOf $ hash (Set [a])
          let set_2 = getResultOf $ hash (Set [a, a])
          set_1 `shouldNotBe` set_2
      describe "Unique" $ do
        it "should soon be tested" $ do
          1 `shouldBe` 1
      describe "Composite" $ do
        describe "Tuple" $ do
          it "order should matter" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let tuple_1 = getResultOf $ hash (Composite Tuple [a, b])
            let tuple_2 = getResultOf $ hash (Composite Tuple [b, a])
            tuple_1 `shouldNotBe` tuple_2
        describe "Either" $ do
          it "order should not matter" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let tuple_1 = getResultOf $ hash (Composite Either [a, b])
            let tuple_2 = getResultOf $ hash (Composite Either [b, a])
            tuple_1 `shouldBe` tuple_2
        describe "Functions" $ do
          it "order should matter" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let tuple_1 = getResultOf $ hash (Composite Function [a, b])
            let tuple_2 = getResultOf $ hash (Composite Function [b, a])
            tuple_1 `shouldNotBe` tuple_2
        it "All should hash diff" $ do
          let a = Thing (Name "a")
          let b = Thing (Name "b")
          let tuple_1 = getResultOf $ hash (Composite Function [a, b])
          let tuple_2 = getResultOf $ hash (Composite Tuple [a, b])
          let tuple_3 = getResultOf $ hash (Composite Either [a, b])
          tuple_1 `shouldNotBe` tuple_2
          tuple_2 `shouldNotBe` tuple_3
          tuple_1 `shouldNotBe` tuple_3  
      describe "Refined" $ do
          it "should take prop into account" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")
            let r1 = getResultOf $ hash $ Refined a b
            let r2 = getResultOf $ hash $ Refined a c
            r1 `shouldNotBe` r2
      -- describe "Placeholder" $ do
      --     it "should ignore different namings" $ do
      --       let a = Thing (Name "a")
      --       let b = Thing (Name "b")
      --       let p1 = getResultOf $ hash $ Placeholder (Name "a") Label a
      --       let p2 = getResultOf $ hash $ Placeholder (Name "b") Label a
      --       p1 `shouldBe` p2
            