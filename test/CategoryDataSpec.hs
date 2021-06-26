module CategoryDataSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import CategoryData
import CategoryCore

instance Arbitrary Category where
    arbitrary = oneof [arbitraryThing]
        where arbitraryThing = do
                  name <- arbitrary
                  return $ Thing name

spec :: Spec
spec = do
  describe "level" $ do
    it "(things) should have a level of zero" $ do
        let thing = Thing "thing"
        level thing `shouldBe` (0 :: Int)
    it "(higher categories) should have a level of 1 more than their parts" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let higher_category = Composite "all" Higher [thing, thing2]
        level higher_category `shouldBe` (1 :: Int)
    it "(Product categories) should have a level of their components" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let product_category = Composite "all" Product [thing, thing2]
        level product_category `shouldBe` (0 :: Int)
    it "(Sum categories) should have a level of their components" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let sum_category = Composite "all" Sum [thing, thing2]
        level sum_category `shouldBe` (0 :: Int)
    it "(Morphisms) should have a level of their components" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let morphism = Morphism "1_2" thing thing2
        level morphism `shouldBe` (0 :: Int)
  describe "has" $ do
    it "(things) should make equal things have each other" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"

        has thing thing `shouldBe` True
        has thing thing2 `shouldBe` False
        has thing2 thing `shouldBe` False
    it "(things) should make things have equal singular algebraic types" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"

        let product_things = Composite "pThings" Product [thing]
        has thing product_things `shouldBe` True
        has thing2 product_things `shouldBe` False
        let product_things = Composite "pThings" Product [thing]
        has thing product_things `shouldBe` True
        has thing2 product_things `shouldBe` False
    it "(morphisms) properly checks simple morphisms" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let a_c = Morphism "a_c" a c
        let b_c = Morphism "b_c" b c

        has a_b a_b `shouldBe` True
        has a_b a_c `shouldBe` False
        has a_b b_c `shouldBe` False
    it "(morphisms) properly checks chain morphisms" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let a_c = Morphism "a_c" a c
        let b_c = Morphism "b_c" b c
        let a_b_c = Morphism "a_b_c" a b_c

        has a_c a_b_c `shouldBe` True
    it "(Product) should make product of things have only a product of those things" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let thing3 = Thing "thing3"

        let product_things = Composite "pThings" Product [thing, thing2]
        has product_things thing `shouldBe` False
        has product_things thing2 `shouldBe` False
        has product_things thing3 `shouldBe` False
        has product_things product_things `shouldBe` True
        has product_things (Composite "pThings" Product [thing, thing3]) `shouldBe` False
    it "(Product) should ignore products of length 1" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"

        let product_things = Composite "pThings" Product [thing]
        has product_things thing `shouldBe` True
        has product_things thing2 `shouldBe` False
        has product_things product_things `shouldBe` True
        has product_things (Composite "pThings" Product [thing, thing2]) `shouldBe` False
    it "(Sum) should make a sum of things have each of those things" $ do
        let thing = Thing "thing"
        let thing2 = Thing "thing2"
        let thing3 = Thing "thing3"

        let sum_things = Composite "pThings" Sum [thing, thing2]
        has sum_things thing `shouldBe` True
        has sum_things thing2 `shouldBe` True
        has sum_things thing3 `shouldBe` False
        has sum_things sum_things `shouldBe` True
        has sum_things (Composite "pThings" Sum [thing, thing3]) `shouldBe` False
        has sum_things (Composite "pThings" Sum [thing, thing2, thing3]) `shouldBe` True
    it "(Composition) should only check input output of composite morphisms" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let b_c = Morphism "b_c" b c
        let a_c = Morphism "a_c" a c

        let composite_morphism = Composite "a_bb_c" Composition [a_b, b_c]

        has composite_morphism a_c `shouldBe` True
        has a_c composite_morphism `shouldBe` True
    it "(Sumposition) should pass it on to any of the interal foos" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let b_c = Morphism "b_c" b c
        let a_c = Morphism "a_c" a c

        let sumposite_morphism = Composite "a_bb_c" Sumposition [a_b, b_c]

        has sumposite_morphism a_b `shouldBe` True
        has sumposite_morphism b_c `shouldBe` True
        has sumposite_morphism a_c `shouldBe` False
    it "(Higher) should have each of its inner categories" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let b_c = Morphism "b_c" b c
        let a_c = Morphism "a_c" a c

        let higher_category = Composite "all" Higher [a,b,a_b,b_c]

        has higher_category a `shouldBe` True
        has higher_category b `shouldBe` True
        has higher_category c `shouldBe` False
        has higher_category a_b `shouldBe` True
        has higher_category b_c `shouldBe` True
        has higher_category a_c `shouldBe` False
    it "(Placeholder) should be contained by its category" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Morphism "a_b" a b
        let b_c = Morphism "b_c" b c
        let a_c = Morphism "a_c" a c

        let higher_category = Composite "all" Higher [a,b,a_b,b_c]
        let ph_hc = Placeholder "ph-all" 0 higher_category

        has higher_category ph_hc `shouldBe` True
        has ph_hc higher_category  `shouldBe` False
    describe "call" $ do
        it "(NonMorphism) should return Nothing" $ do
            let a = Thing "a"
            call a a `shouldBe` Nothing
        it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
            let a = Thing "a"
            let b = Thing "b"

            let a_b = Morphism "a_b" a b
            call a_b a `shouldBe` Just b
            call a_b b `shouldBe` Nothing
        it "(Composite Composition) should handle chains right" $ do
            let a = Thing "a"
            let b = Thing "b"
            let c = Thing "c"

            let a_b = Morphism "a_b" a b
            let b_c = Morphism "b_c" b c
            let a_c = Morphism "a_c" a c

            let composite_morphism = Composite "a_bb_c" Composition [a_b, b_c]
            
            call composite_morphism a `shouldBe` Just c
            call composite_morphism b `shouldBe` Nothing
            call composite_morphism c `shouldBe` Nothing
        it "(Composite Sumposition) should handle sums right" $ do
            let a = Thing "a"
            let b = Thing "b"
            let c = Thing "c"

            let a_b = Morphism "a_b" a b
            let b_c = Morphism "b_c" b c
            let a_c = Morphism "a_c" a c

            let sumposite_morphism = Composite "a_bb_c" Sumposition [a_b, b_c, a_c]
            
            call sumposite_morphism a `shouldBe` Just b
            call sumposite_morphism b `shouldBe` Just c
            call sumposite_morphism c `shouldBe` Nothing

--   quickCheck ((\s -> isThing s == isThing s) :: Category -> Bool)