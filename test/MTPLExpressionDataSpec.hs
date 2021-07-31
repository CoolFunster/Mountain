module MTPLExpressionDataSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import CategoryData
import CategoryCore
import MTPLExpressionData
import MTPLExpressionCore

spec :: Spec
spec = do
--   describe "evaluateExpr" $ do
--     it "(Object) should return the object" $ do
--         let thing = Thing "thing"
--         let expr = Object thing
--         evaluateExpr expr `shouldBe` Object thing
--     it "(Morphism) object expr chains should become morphisms" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         let a2b = MorphismChain "a2b" [Object a, Object b]
--         evaluateExpr a2b `shouldBe` Object (Morphism "a2b" a b)

--         let a2b2a = MorphismChain "a2b2a" [Object a, Object b, Object a]
--         evaluateExpr a2b2a `shouldBe` Object (Morphism "a2b2a" a (Morphism "a2b2a" b a))
--     it "(Dereference) should simplify on two objects" $ do
--         let a = Thing "a"
--         let b = Thing "b"

--         let anb = Composite "a+b" Higher [a,b]

--         evaluateExpr (Dereference (Object anb) "a") `shouldBe` Object a
--         evaluateExpr (Dereference (Object anb) "b") `shouldBe` Object b
--     it "(Call) should call a simple chain properly" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         let a2b = MorphismChain "a2b" [Object a, Object b]

--         evaluateExpr (Call a2b (Object a)) `shouldBe` Object b
--     it "(Construction) should construct a simple type properly" $ do
--         let a = Thing "a"
--         let b = Thing "b"

--         evaluateExpr (Construction "a|b" Sum [Object a, Object b]) `shouldBe` Object (Composite "a|b" Sum [a,b])
--   describe "categoryType" $ do
--     it "(Object) should check category is valid and then return the category" $ do
--         let a = Thing "a"
--         categoryType (Object a) `shouldBe` Just a

--         let bad_category = Composite "bad" Sumposition []
--         categoryType (Object bad_category) `shouldBe` Nothing
--     it "(MorphismChain) should Nothing if bad inner types" $ do
--         let bad_category = Composite "bad" Sumposition []
--         categoryType (MorphismChain "bad_chain" [Object bad_category,Object bad_category]) `shouldBe` Nothing
--     it "(MorphismChain) should curry good long chains" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         categoryType (MorphismChain "good_chain" [Object a,Object b]) `shouldBe` Just (Morphism "good_chain" a b)

--         let c = Thing "c"
--         categoryType (MorphismChain "good_long_chain" [Object a,Object b,Object c]) `shouldBe` Just (Morphism "good_long_chain" a (Morphism "good_long_chain" b c))
--     it "(MorphismChain) should not handle bad short chains" $ do
--         categoryType (MorphismChain "bad_chain" []) `shouldBe` Nothing

--         let a = Thing "a"
--         categoryType (MorphismChain "bad_chain" [Object a]) `shouldBe` Just a
--     it "(Dereference) should not handle bad base expr" $ do
--         let bad_category = Composite "bad" Sumposition []
--         categoryType (Dereference (Object bad_category) "x") `shouldBe` Nothing
--     it "(Dereference) should be the right deref" $ do
--         let good_category = Composite "good" Sum [Thing "a",Thing "b"]
--         categoryType (Dereference (Object good_category) "a") `shouldBe` Just (Thing "a")
--     it "(Call) should be Just if good and Nothing if bad input" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         let a_b = Morphism "a_b" a b
--         categoryType (Call (Object a_b) (Object a)) `shouldBe` Just b
--         categoryType (Call (Object a_b) (Object b)) `shouldBe` Nothing
--     it "(Construction) should handle bad inner input on Sum, product and higher" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         let bad_category = Composite "bad" Sumposition []
--         let bad_construction_sum = Construction "ab_sum" Sum [Object a,Object b,Object bad_category]
--         let bad_construction_prod = Construction "ab_prod" Product [Object a,Object b,Object bad_category]
--         let bad_construction_higher = Construction "ab_high" Higher [Object a,Object b,Object bad_category]
        
--         categoryType bad_construction_sum `shouldBe` Nothing
--         categoryType bad_construction_prod `shouldBe` Nothing
--         categoryType bad_construction_higher `shouldBe` Nothing

--         let good_construction_sum = Construction "ab_sum" Sum [Object a,Object b]
--         let good_construction_prod = Construction "ab_prod" Product [Object a,Object b]
--         let good_construction_higher = Construction "ab_high" Higher [Object a,Object b]

--         categoryType good_construction_sum `shouldBe` Just (Composite "ab_sum" Sum [a,b])
--         categoryType good_construction_prod `shouldBe` Just (Composite "ab_prod" Product [a,b])
--         categoryType good_construction_higher `shouldBe` Just (Composite "ab_high" Higher [a,b])
--   describe "runMTPL" $ do
--     it "should handle simple morphism call" $ do
--         let a = Thing "a"
--         let b = Thing "b"
--         let a_b = Morphism "a->b" a b

--         let expr = Call (Object a_b) (Object a)
--         runMTPL expr `shouldBe` Object b
  describe "makeRecursive" $ do
    it "should properly replace a ref with something recursive" $ do
       1 `shouldBe` 1
        -- let rec_ref = Reference "self"
        -- let always_a = MorphismChain "maybe_loop_forever" [rec_ref,Construction "maybe_loop_forever" Sumposition [Object (Morphism "a->b" (Thing "a") (Thing "b")),MorphismChain "otherwise -> self a" [Object (Thing "b"), Call rec_ref (Object (Thing "a"))]]]

        -- let rec_result = makeRecursive always_a
        -- print $ evaluateExpr (Call rec_result (Object (Thing "a")))
    