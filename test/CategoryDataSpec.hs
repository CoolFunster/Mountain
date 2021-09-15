module CategoryDataSpec (spec) where

import Test.Hspec
import CategoryData
import CategoryCore

import Categories.Numbers (natural)

import Data.Maybe (fromJust)

-- TODO Split into respective files

spec :: Spec
spec = do
    describe "level" $ do
        it "(things) should have a level of zero" $ do
            let thing = Thing (Name "thing")
            level thing `shouldBe` Just (0 :: Int)
        it "(higher categories) should have a level of 1 more than their parts" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let higher_category = Composite (Name "all") Higher [thing, thing2]
            level higher_category `shouldBe` Just (1 :: Int)
        it "(Product categories) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let product_category = Composite (Name "all") Product [thing, thing2]
            level product_category `shouldBe` Just (0 :: Int)
        it "(Sum categories) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let sum_category = Composite (Name "all") Sum [thing, thing2]
            level sum_category `shouldBe` Just (0 :: Int)
        it "(Morphisms) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let morphism = Morphism (Name "1_2") thing thing2
            level morphism `shouldBe` Just (0 :: Int)
    describe "has" $ do
        it "(things) should make equal things have each other" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")

            has thing thing `shouldBe` True
            has thing thing2 `shouldBe` False
            has thing2 thing `shouldBe` False
        it "(things) should make things have equal singular algebraic types" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")

            let product_things = Composite (Name "pThings") Product [thing]
            has thing product_things `shouldBe` True
            has thing2 product_things `shouldBe` False
            let product_things = Composite (Name "pThings") Product [thing]
            has thing product_things `shouldBe` True
            has thing2 product_things `shouldBe` False
        it "(morphisms) properly checks simple morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let a_c = Morphism (Name "a_c") a c
            let b_c = Morphism (Name "b_c") b c

            has a_b a_b `shouldBe` True
            has a_b a_c `shouldBe` False
            has a_b b_c `shouldBe` False
        it "(morphisms) does not allow intermediate morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let a_c = Morphism (Name "a_c") a c
            let b_c = Morphism (Name "b_c") b c
            let a_b_c = Morphism (Name "a_b_c") a b_c

            has a_c a_b_c `shouldBe` False
        it "(Product) should make product of things have only a product of those things" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let thing3 = Thing (Name "thing3")

            let product_things = Composite (Name "pThings") Product [thing, thing2]
            has product_things thing `shouldBe` False
            has product_things thing2 `shouldBe` False
            has product_things thing3 `shouldBe` False
            has product_things product_things `shouldBe` True
            has product_things (Composite (Name "pThings") Product [thing, thing3]) `shouldBe` False
        it "(Product) should ignore products of length 1" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")

            let product_things = Composite (Name "pThings") Product [thing]
            has product_things thing `shouldBe` True
            has product_things thing2 `shouldBe` False
            has product_things product_things `shouldBe` True
            has product_things (Composite (Name "pThings") Product [thing, thing2]) `shouldBe` False
        it "(Sum) should make a sum of things have each of those things" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let thing3 = Thing (Name "thing3")

            let sum_things = Composite (Name "pThings") Sum [thing, thing2]
            has sum_things thing `shouldBe` True
            has sum_things thing2 `shouldBe` True
            has sum_things thing3 `shouldBe` False
            has sum_things sum_things `shouldBe` True
            has sum_things (Composite (Name "pThings") Sum [thing, thing3]) `shouldBe` False
            has sum_things (Composite (Name "pThings") Sum [thing, thing2, thing3]) `shouldBe` True
        it "(Composition) should only check input output of composite morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c

            let composite_morphism = Composite (Name "a_bb_c") Composition [a_b, b_c]

            has composite_morphism a_c `shouldBe` True
            has a_c composite_morphism `shouldBe` True
        it "(Sumposition) should pass it on to any of the interal foos" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c

            let sumposite_morphism = Composite (Name "a_bb_c") Sumposition [a_b, b_c]

            has sumposite_morphism a_b `shouldBe` True
            has sumposite_morphism b_c `shouldBe` True
            has sumposite_morphism a_c `shouldBe` False
        it "(Higher) should have each of its inner categories" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")
            let d = Thing (Name "d")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c
            let a_d = Morphism (Name "a_c") a d

            let higher_category = Composite (Name "all") Higher [a,b,a_b,b_c]

            has higher_category a `shouldBe` True
            has higher_category b `shouldBe` True
            has higher_category c `shouldBe` True
            has higher_category a_b `shouldBe` True
            has higher_category b_c `shouldBe` True
            has higher_category a_c `shouldBe` True
            has higher_category d `shouldBe` False
            has higher_category a_d `shouldBe` False
        it "(Placeholder) should be contained by its category" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c

            let higher_category = Composite (Name "all") Higher [a,b,a_b,b_c]
            let ph_hc = Placeholder (Name "ph-all") (Just 0) higher_category

            has higher_category ph_hc `shouldBe` True
            has ph_hc higher_category  `shouldBe` False
    describe "call" $ do
        it "(NonMorphism) should return Nothing" $ do
            let a = Thing (Name "a")
            call a a `shouldBe` Nothing
        it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")

            let a_b = Morphism (Name "a_b") a b
            call a_b a `shouldBe` Just b
            call a_b b `shouldBe` Nothing
        it "(Composite Composition) should handle chains right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c

            let composite_morphism = Composite (Name "a_bb_c") Composition [a_b, b_c]
            
            call composite_morphism a `shouldBe` Just c
            call composite_morphism b `shouldBe` Nothing
            call composite_morphism c `shouldBe` Nothing
        it "(Composite Sumposition) should handle sums right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism (Name "a_b") a b
            let b_c = Morphism (Name "b_c") b c
            let a_c = Morphism (Name "a_c") a c

            let sumposite_morphism = Composite (Name "a_bb_c") Sumposition [a_b, b_c, a_c]
            
            call sumposite_morphism a `shouldBe` Just b
            call sumposite_morphism b `shouldBe` Just c
            call sumposite_morphism c `shouldBe` Nothing
    describe "validCategory" $ do
        it "(Composite) should return False for empty sumposition/composition" $ do
            let bad_category = Composite (Name "bad") Composition []
            validCategory bad_category `shouldBe` False
            let bad_category2 = Composite (Name "bad") Sumposition []
            validCategory bad_category2 `shouldBe` False
    describe "execute" $ do
        it "(Thing) should just return the thing" $ do
            let a = Thing (Name "a")
            execute a `shouldBe` a
        it "(Composite) should just return the composite" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let ab = Composite (Name "ab") Product [a,b]
            execute ab `shouldBe` ab
        it "(Morphism) should just return the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Morphism (Name "a->b") a b
            execute a2b `shouldBe` a2b
        it "(Placeholder) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite (Name "<ab>") Higher [a,b]
            let x_elem_a_b = Placeholder (Name "x") (Just 0) a_b
            execute x_elem_a_b `shouldBe` x_elem_a_b
        it "(Placeholder) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite (Name "<ab>") Higher [a,b]
            let x_elem_a_b = Placeholder (Name "x") (Just 0) a_b
            execute x_elem_a_b `shouldBe` x_elem_a_b
        it "(MorphismCall) should just call the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Morphism (Name "a->b") a b
            let a2b_on_a = MorphismCall a2b a
            execute a2b_on_a `shouldBe` b
        it "(RecursiveCategory) should just return itself" $ do
            let nat = makeRecursiveCategory (Name "self") (Composite (Name "Nat") Sum [Thing (Name "0"), Morphism (Name "succ") valid (Special (Name "self") Reference)])
            execute nat `shouldBe` unfold Recursive nat
        it "(RecursiveCategory) should have all different elements" $ do
            let nat = makeRecursiveCategory (Name "self") (Composite (Name "Nat") Sum [Thing (Name "0"), Morphism (Name "succ") valid (Special (Name "self") Reference)])
            nat `has` Thing (Name "0") `shouldBe` True
            nat `has` Morphism (Name "succ") valid (Thing (Name "0")) `shouldBe` True
            nat `has` Morphism (Name "succ") valid (Morphism (Name "succ") valid (Thing (Name "0"))) `shouldBe` True
        it "(RecursiveCategory) should be callable in a morphism call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let simple_ab = makeRecursiveCategory (Name "simple_ab") (Composite (Name "ab") Sumposition [Morphism (Name "ab1") a b, Morphism (Name "ab2") b (MorphismCall Special{name=Name "simple_ab", special_type=Reference} a)])
            -- print $ inner_expr simple_ab
            simplify (MorphismCall simple_ab b) `shouldBe` MorphismCall simple_ab b
            simplify (MorphismCall simple_ab a) `shouldBe` MorphismCall simple_ab a
            execute (MorphismCall simple_ab a) `shouldBe` b
            execute (MorphismCall simple_ab b) `shouldBe` b
    describe "sample" $ do
        it "(Thing) requesting a thing of level 0 should return a thing" $ do
            let a = Thing (Name "a")
            sample (Placeholder (Name "something") (Just 0) a) `shouldBe` a
        it "(Higher) requesting a thing of a recursive type should return that thing" $ do
            sample (Placeholder (Name "x") (Just 0) natural) `shouldBe` fromJust (dereference (Name "zero") natural)
    describe "dereference" $ do
        it "should handle indices on composites well" $ do
            let composite_category = Composite (Name "something") Product [Thing (Name "a"), Thing (Name "b")]

            dereference (Index 0) composite_category `shouldBe` Just (Thing (Name "a"))
            dereference (Index 1) composite_category `shouldBe` Just (Thing (Name "b"))
            dereference (Name "a") composite_category `shouldBe` Just (Thing (Name "a"))
            dereference (Name "b") composite_category `shouldBe` Just (Thing (Name "b"))
            

            