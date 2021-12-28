module CategoryDataSpec (spec) where

import Test.Hspec
import CategoryData
import CategoryCore

import FrontEnds.Textual.V1.CategoryWriter (categoryToText, categoryToStr)

import Data.Maybe (fromJust)
import Language.Python.Common.AST (Expr(Tuple))
import CategoryCore (importCategories)
import FrontEnds.Textual.V1.CategoryParser (parseCategoryString)

-- TODO Split into respective files

nat::Category
nat = Label{
    name=Name "Nat",
    target=Composite{
        composition_type=Sum,
        inner=[
            Thing (Name "0"),
            Morphism valid (Reference (Name "Nat"))
        ]
    }
}

spec :: Spec
spec = do
    describe "level" $ do
        it "(things) should have a level of zero" $ do
            let thing = Thing (Name "thing")
            level thing `shouldBe` Specific 0
        it "(higher categories) should have a level of 1 more than their parts" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let higher_category = Composite Higher [thing, thing2]
            level higher_category `shouldBe` Specific 1
        it "(Product categories) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let product_category = Composite Product [thing, thing2]
            level product_category `shouldBe` Specific 0
        it "(Sum categories) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let sum_category = Composite Sum [thing, thing2]
            level sum_category `shouldBe` Specific 0
        it "(Morphisms) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let morphism = Morphism thing thing2
            level morphism `shouldBe` Specific 0
        it "(Recursive) recursive categories should have an appropriate level" $ do
            let thing = Thing (Name "thing")
            let recursive_cat = Label{name=Name "self", target=Composite{composition_type=Product,inner=[thing, Reference{name=Name "self"}]}}
            isRecursiveCategory recursive_cat `shouldBe` True
            level recursive_cat `shouldBe` Specific 1
        it "(Example 1) should have correct level" $ do
            let result = parseCategoryString "given x@(|zero:(),nonzero:(head:(),rest:nat_data:|zero:(),nonzero:(head:(),rest:$nat_data)|)|) -> return (|zero:(),nonzero:(head:(),rest:nat_data:|zero:(),nonzero:(head:(),rest:$nat_data)|)|)::(((),$x))"
            level result `shouldBe` Specific 1
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

            let product_things = Composite Product [thing]
            has thing product_things `shouldBe` True
            has thing2 product_things `shouldBe` False
            let product_things = Composite Product [thing]
            has thing product_things `shouldBe` True
            has thing2 product_things `shouldBe` False
        it "(morphisms) properly checks simple morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let a_c = Morphism a c
            let b_c = Morphism b c

            has a_b a_b `shouldBe` True
            has a_b a_c `shouldBe` False
            has a_b b_c `shouldBe` False
        it "(morphisms) does not allow intermediate morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let a_c = Morphism a c
            let b_c = Morphism b c
            let a_b_c = Morphism a b_c

            has a_c a_b_c `shouldBe` False
        it "(Product) should make product of things have only a product of those things" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let thing3 = Thing (Name "thing3")

            let product_things = Composite Product [thing, thing2]
            has product_things thing `shouldBe` False
            has product_things thing2 `shouldBe` False
            has product_things thing3 `shouldBe` False
            has product_things product_things `shouldBe` True
            has product_things (Composite Product [thing, thing3]) `shouldBe` False
        it "(Product) should ignore products of length 1" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")

            let product_things = Composite Product [thing]
            has product_things thing `shouldBe` True
            has product_things thing2 `shouldBe` False
            has product_things product_things `shouldBe` True
            has product_things (Composite Product [thing, thing2]) `shouldBe` False
        it "(Sum) should make a sum of things have each of those things" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let thing3 = Thing (Name "thing3")

            let sum_things = Composite Sum [thing, thing2]
            has sum_things thing `shouldBe` True
            has sum_things thing2 `shouldBe` True
            has sum_things thing3 `shouldBe` False
            has sum_things sum_things `shouldBe` True
            has sum_things (Composite Sum [thing, thing3]) `shouldBe` False
            has sum_things (Composite Sum [thing, thing2, thing3]) `shouldBe` True
        it "(Composition) should only check input output of composite morphisms" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c

            let composite_morphism = Composite Composition [a_b, b_c]

            has composite_morphism a_c `shouldBe` True
            has a_c composite_morphism `shouldBe` True
        it "(Sumposition) should pass it on to any of the interal foos" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c

            let sumposite_morphism = Composite Sumposition [a_b, b_c]

            has sumposite_morphism a_b `shouldBe` True
            has sumposite_morphism b_c `shouldBe` True
            has sumposite_morphism a_c `shouldBe` False
        it "(Higher) should have each of its inner categories" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")
            let d = Thing (Name "d")

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c
            let a_d = Morphism a d

            let higher_category = Composite Higher [a,b,a_b,b_c]

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

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c

            let higher_category = Composite Higher [a,b,a_b,b_c]
            let ph_hc = Placeholder (Name "ph-all") (Specific 0) higher_category

            has higher_category ph_hc `shouldBe` True
            has ph_hc higher_category  `shouldBe` False
        it "(example) should have" $ do
            let cat1 = parseCategoryString "nat_data:|(),((),$nat_data)|"
            let cat2 = parseCategoryString "((),nat_data:|(),((),$nat_data)|)"
            cat1 `tracedHas` cat2 `shouldBe` True
    describe "isSubstitutable" $ do
        it "(example 1) should be valid" $ do
            let morph_input = parseCategoryString "x@(((),nat_data:|(),((),$nat_data)|))"
            let arg = parseCategoryString "x@(((),((),nat_data:|(),((),$nat_data)|)))"
            morph_input `tracedHas` arg `shouldBe` False
            arg `isSubstitutable` morph_input `shouldBe` False
    describe "call" $ do
        it "(NonMorphism) should return Nothing" $ do
            let a = Thing (Name "a")
            call a a `shouldBe` Nothing
        it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")

            let a_b = Morphism a b
            call a_b a `shouldBe` Just b
            call a_b b `shouldBe` Nothing
        it "(Composite Composition) should handle chains right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c

            let composite_morphism = Composite Composition [a_b, b_c]

            call composite_morphism a `shouldBe` Just c
            call composite_morphism b `shouldBe` Nothing
            call composite_morphism c `shouldBe` Nothing
        it "(Composite Sumposition) should handle sums right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Morphism a b
            let b_c = Morphism b c
            let a_c = Morphism a c

            let sumposite_morphism = Composite Sumposition [a_b, b_c, a_c]

            call sumposite_morphism a `shouldBe` Just b
            call sumposite_morphism b `shouldBe` Just c
            call sumposite_morphism c `shouldBe` Nothing
    describe "isValidCategory" $ do
        it "(Composite) should return False for empty sumposition/composition" $ do
            let bad_category = Composite Composition []
            isValidCategory bad_category `shouldBe` False
            let bad_category2 = Composite Sumposition []
            isValidCategory bad_category2 `shouldBe` False
    describe "execute" $ do
        it "(Thing) should just return the thing" $ do
            let a = Thing (Name "a")
            result <- execute a
            result `shouldBe` a
        it "(Composite) should just return the composite" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let ab = Composite Product [a,b]
            result <- execute ab
            result `shouldBe` ab
        it "(Morphism) should just return the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Morphism a b
            result <- execute a2b
            result `shouldBe` a2b
        it "(Placeholder) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Higher [a,b]
            let x_elem_a_b = Placeholder (Name "x") (Specific 0) a_b
            result <- execute x_elem_a_b
            result `shouldBe` x_elem_a_b
        it "(Placeholder) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Higher [a,b]
            let x_elem_a_b = Placeholder (Name "x") (Specific 0) a_b
            result <- execute x_elem_a_b
            result `shouldBe` x_elem_a_b
        it "(MorphismCall) should just call the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Morphism a b
            let a2b_on_a = MorphismCall a2b a
            result <- execute a2b_on_a
            result `shouldBe` b
        it "(RecursiveCategory) should just return itself" $ do
            result <- execute nat
            result `shouldBe` unfold Recursive nat
        it "(RecursiveCategory) should have each of the component elements" $ do
            nat `has` Thing (Name "0") `shouldBe` True
            nat `has` Morphism valid (Thing (Name "0")) `shouldBe` True
            nat `has` Morphism valid (Morphism valid (Thing (Name "0"))) `shouldBe` True
        it "(RecursiveCategory) should be callable in a morphism call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let simple_ab = Label{
                name=Name "ab",
                target=Composite {
                    composition_type=Sumposition,
                    inner=[
                        Morphism a b,
                        Morphism b (MorphismCall (Reference (Name "ab")) a)
                    ]
                }
            }
            let unfolded_simple_ab = unfold Recursive simple_ab
            -- print $ inner_expr simple_ab
            simplify (MorphismCall unfolded_simple_ab b) `shouldBe` MorphismCall unfolded_simple_ab b
            simplify (MorphismCall unfolded_simple_ab a) `shouldBe` MorphismCall unfolded_simple_ab a
            result <- execute (MorphismCall unfolded_simple_ab a)
            result `shouldBe` b
    describe "dereference" $ do
        it "should handle indices on composites well" $ do
            let composite_category = Composite Product [Thing (Name "a"), Thing (Name "b")]
            dereference (Index 0) composite_category `shouldBe` Just (Thing (Name "a"))
            dereference (Index 1) composite_category `shouldBe` Just (Thing (Name "b"))
            dereference (Name "a") composite_category `shouldBe` Just (Thing (Name "a"))
            dereference (Name "b") composite_category `shouldBe` Just (Thing (Name "b"))
    describe "unfold" $ do
        it "should unfold recursive labels" $ do
            let simple_ab = Label{
                name=Name "ab",
                target=Composite {
                    composition_type=Product,
                    inner=[
                        Thing (Name "0"),
                        Reference (Name "ab")
                    ]
                }
            }
            unfold Recursive simple_ab `shouldBe` Composite {composition_type = Product, inner = [Thing {name = Name "0"},Label {name = Name "ab", target = Composite {composition_type = Product, inner = [Thing {name = Name "0"},Reference {name = Name "ab"}]}}]}
    describe "importCategories" $ do
        it "should passthrough things" $ do
            let test_item = Thing{name=Name "name"}
            result <- importCategories test_item
            result `shouldBe` test_item
        it "should import intermediate morphisms " $ do
            let test_item = IntermediateMorphism [MorphismTerm{m_type=CategoryData.Import, m_category=Label{name=Name "l", target=Reference{name=Name "List"}}}]
            result <- importCategories test_item
            result `shouldBe` IntermediateMorphism {chain = [MorphismTerm {m_type = Definition, m_category = Label {name = Name "l", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "list_type", ph_level = AnyLevel, ph_category = Special {special_type = Universal}}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Sum, inner = [Label {name = Name "empty", target = Thing {name = Name "empty_list"}},Label {name = Name "nonempty", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Reference {name = Name "list_type"}},Label {name = Name "tail", target = MorphismCall {base_morphism = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}]}}]}}]}}}]}
    describe "RecursiveCategory" $ do
        it "(isRecursiveCat) example 1 " $ do
            let parsed = parseCategoryString "nat_data:|zero:(),nonzero:(head:(),rest:$nat_data)|"
            isRecursiveCategory parsed `shouldBe` True