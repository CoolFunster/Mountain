{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module CategoryDataSpec (spec) where

import Test.Hspec
import CategoryData

-- TODO Split into respective files

nat::Category
nat = Placeholder{
    name=Name "Nat",
    placeholder_type=Label,
    placeholder_category=Composite{
        composite_type=Union,
        inner_categories=[
            Thing (Name "0"),
            Composite Tuple [Thing (Name "S"), Reference (Name "Nat")]
        ]
    }
}

simpleRecursiveCat::Category
simpleRecursiveCat = Placeholder{
    name=Name "self",
    placeholder_type=Label,
    placeholder_category=Composite{composite_type=Tuple,inner_categories=[
        Thing (Name "thing"), 
        Reference{name=Name "self"}]
    }
}

spec :: Spec
spec = do
    describe "checkAST" $ do
        it "should check properly for things" $ do
            checkAST or isThing (Thing (Name "x")) `shouldBe` True
        it "should check properly for Composites" $ do
            checkAST or isThing (Composite Tuple [Thing (Name "x")]) `shouldBe` True
        it "should work for composites with multiple categories" $ do
            checkAST or (isReferenceOfName (Name "self")) Composite{composite_type=Tuple,inner_categories=[Thing (Name "thing"), Reference{name=Name "self"}]} `shouldBe` True
        it "should check properly for Placeholders" $ do
            checkAST or isThing (Placeholder Unnamed Label (Thing (Name "x"))) `shouldBe` True
        it "should check properly for Refined" $ do
            checkAST or isThing (Refined (Thing (Name "x")) valid) `shouldBe` True
    describe "isLabelOfName" $ do
        it "Should work for labels with a particular name" $ do
            isLabelOfName (Name "x") (Thing (Name "x")) `shouldBe` False
            isLabelOfName (Name "x") (Placeholder (Name "x") Label valid) `shouldBe` True
            isLabelOfName (Name "x") (Placeholder (Name "x") Element valid) `shouldBe` False
    describe "isRecursiveCategory" $ do
        it "Simple recursion should be identified" $ do
            isRecursiveCategory simpleRecursiveCat `shouldBe` True
        it "Nats should be recursive" $ do
            isRecursiveCategory nat `shouldBe` True
    describe "level" $ do
        it "(things) should have a level of zero" $ do
            let thing = Thing (Name "thing")
            level thing `shouldBe` Left (Specific 0)
        it "(Union categories) should have a level of 1 more than their parts" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let higher_category = Composite Union [thing, thing2]
            level higher_category `shouldBe` Left (Specific 1)
        it "(Tuple categories) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let product_category = Composite Tuple [thing, thing2]
            level product_category `shouldBe` Left (Specific 0)
        it "(Morphisms) should have a level of their components" $ do
            let thing = Thing (Name "thing")
            let thing2 = Thing (Name "thing2")
            let morphism = Composite Function [thing, thing2]
            level morphism `shouldBe` Left (Specific 0)
        it "(Recursive) recursive categories should have an appropriate level" $ do
            let thing = Thing (Name "thing")
            let recursive_cat = Placeholder{
                name=Name "self",
                placeholder_type=Label,
                placeholder_category=Composite{composite_type=Tuple,inner_categories=[thing, Reference{name=Name "self"}]}}
            level recursive_cat `shouldBe` Left (Specific 1)
    describe "has" $ do
        let thing = Thing (Name "thing")
        let thing2 = Thing (Name "thing2")
        let thing3 = Thing (Name "thing3")
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")
        let a_b = Composite Function [a, b]
        let b_c = Composite Function [b, c]
        let a_c = Composite Function [a, c]
        let a_d = Composite Function [a, d]
        it "(things) should make equal things have each other" $ do
            has thing thing `shouldBe` Valid True
            has thing thing2 `shouldBe` Valid False
            has thing2 thing `shouldBe` Valid False
        it "(things) should make things have equal singular algebraic types" $ do
            let product_things = Composite Tuple [thing]
            has thing product_things `shouldBe` Valid True
            has thing2 product_things `shouldBe` Valid False
            let product_things = Composite Tuple [thing]
            has thing product_things `shouldBe` Valid True
            has thing2 product_things `shouldBe` Valid False
        it "(morphisms) properly checks simple morphisms" $ do
            let a_b = Composite Function [a, b]
            let a_c = Composite Function [a, c]
            let b_c = Composite Function [b, c]

            has a_b a_b `shouldBe` Valid True
            has a_b a_c `shouldBe` Valid False
            has a_b b_c `shouldBe` Valid False
        it "(morphisms) does not allow intermediate morphisms" $ do
            let a_b = Composite Function [a, b]
            let a_c = Composite Function [a, c]
            let b_c = Composite Function [b, c]
            let a_b_c = Composite Function [a, b_c]

            has a_c a_b_c `shouldBe` Valid False
        it "(Product) should make Tuple of things have only a Tuple of those things" $ do
            let product_things = Composite Tuple [thing, thing2]
            has product_things thing `shouldBe` Valid False
            has product_things thing2 `shouldBe` Valid False
            has product_things thing3 `shouldBe` Valid False
            has product_things product_things `shouldBe` Valid True
            has product_things (Composite Tuple [thing, thing3]) `shouldBe` Valid False
        it "(Product) should ignore products of length 1" $ do
            let product_things = Composite Tuple [thing]
            has product_things thing `shouldBe` Valid True
            has product_things thing2 `shouldBe` Valid False
            has product_things product_things `shouldBe` Valid True
            has product_things (Composite Tuple [thing, thing2]) `shouldBe` Valid False
        it "(Union) should make a sum of things have each of those things" $ do
            let sum_things = Composite Union [thing, thing2]
            has sum_things thing `shouldBe` Valid True
            has sum_things thing2 `shouldBe` Valid True
            has sum_things thing3 `shouldBe` Valid False
            has thing sum_things `shouldBe` Valid False
            has thing2 sum_things `shouldBe` Valid False
            has thing3 sum_things `shouldBe` Valid False
            has sum_things sum_things `shouldBe` Valid True
            has sum_things (Composite Union [thing, thing3]) `shouldBe` Valid False
            has sum_things (Composite Union [thing, thing2, thing3]) `shouldBe` Valid False
            has (Composite Union [thing, thing2, thing3]) sum_things `shouldBe` Valid True
        it "(Composition) should only check input output of composite morphisms" $ do
            let composite_morphism = Composite Composition [a_b, b_c]

            has composite_morphism a_c `shouldBe` Valid True
            has a_c composite_morphism `shouldBe` Valid True
        it "(Case) should pass it on to any of the interal foos" $ do
            let sumposite_morphism = Composite Case [a_b, b_c]

            has sumposite_morphism a_b `shouldBe` Valid True
            has sumposite_morphism b_c `shouldBe` Valid True
            has sumposite_morphism a_c `shouldBe` Valid False
        it "(Union) should have each of its inner categories" $ do
            let higher_category = Composite Union [a,b,a_b,b_c]

            has higher_category a `shouldBe` Valid True
            has higher_category b `shouldBe` Valid True
            has higher_category c `shouldBe` Valid False
            has higher_category a_b `shouldBe` Valid True
            has higher_category b_c `shouldBe` Valid True
            has higher_category a_c `shouldBe` Valid False
            has higher_category d `shouldBe` Valid False
            has higher_category a_d `shouldBe` Valid False
        it "(Placeholder) should be contained by its category" $ do
            let higher_category = Composite Union [a,b,a_b,b_c]
            let ph_hc = Placeholder (Name "ph-all") Element higher_category

            has higher_category ph_hc `shouldBe` Valid True
            has ph_hc higher_category  `shouldBe` Valid False
        it "(RecursiveCategory) should have each of the component elements" $ do
            nat `has` Thing (Name "0") `shouldBe` Valid True
            nat `has` Composite Tuple [Thing (Name "S"), Thing (Name "0")] `shouldBe` Valid True
            nat `has` Composite Tuple [Composite Tuple [Thing (Name "S"),Thing (Name "0")]] `shouldBe` Valid True
    describe "call" $ do
        it "(NonMorphism) should return Nothing" $ do
            let a = Thing (Name "a")
            print $ call a a
            isError (call a a) `shouldBe` True
        it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")

            let a_b = Composite Function [a, b]
            call a a_b `shouldBe` Valid b
            isError (call b a_b) `shouldBe` True
        it "(Composite Composition) should handle chains right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Composite Function [a, b]
            let b_c = Composite Function [b, c]
            let a_c = Composite Function [a, c]

            let composite_morphism = Composite Composition [a_b, b_c]

            call a composite_morphism `shouldBe` Valid c
            isError (call b composite_morphism) `shouldBe` True
            isError (call c composite_morphism) `shouldBe` True
        it "(Composite Case) should handle sums right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Composite Function [a, b]
            let b_c = Composite Function [b, c]
            let a_c = Composite Function [a, c]

            let case_function = Composite Case [a_b, b_c]

            call a case_function `shouldBe` Valid b
            call b case_function `shouldBe` Valid c
            isError (call c case_function) `shouldBe` True
    describe "validateCategory" $ do
        it "(Placeholder) should not modify this placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Union [a,b]
            let x_elem_a_b = Placeholder (Name "x") Element a_b
            validateCategory x_elem_a_b `shouldBe` Valid x_elem_a_b
        it "(Placeholder) should not modify this function call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a,b]
            let a2b_on_a = FunctionCall a2b a
            let result = validateCategory a2b_on_a
            result `shouldBe` Valid a2b_on_a
    describe "simplify" $ do
        it "(simplify) should not modify this placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Union [a,b]
            let x_elem_a_b = Placeholder (Name "x") Element a_b
            simplify x_elem_a_b `shouldBe` Valid x_elem_a_b
        it "(simplify) should not modify this function call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a,b]
            let a2b_on_a = FunctionCall a2b a
            let result = simplify a2b_on_a
            result `shouldBe` Valid a2b_on_a
    describe "execute" $ do
        it "(Thing) should just return the thing" $ do
            let a = Thing (Name "a")
            result <- runErrorableT $ execute a
            result `shouldBe` Valid a
        it "(Composite) should just return the composite" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let ab = Composite Tuple [a,b]
            result <- runErrorableT $ execute ab
            result `shouldBe` Valid ab
        it "(Morphism) should just return the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a, b]
            result <- runErrorableT $ execute a2b
            result `shouldBe` Valid a2b
        it "(Placeholder) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Union [a,b]
            let x_elem_a_b = Placeholder (Name "x") Element a_b
            result <- runErrorableT $ execute x_elem_a_b
            result `shouldBe` Valid x_elem_a_b
        it "(FunctionCall) should just call the function" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a,b]
            let a2b_on_a = FunctionCall a2b a
            result <- runErrorableT $ execute a2b_on_a
            result `shouldBe` Valid b
        it "(RecursiveCategory) should just return itself" $ do
            result <- runErrorableT $ execute nat
            result `shouldBe` Valid nat
        it "(RecursiveCategory) should be callable in a morphism call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let simple_ab = Placeholder{
                name=Name "ab",
                placeholder_type=Label,
                placeholder_category=Composite {
                    composite_type=Case,
                    inner_categories=[
                        Composite Function [a,b],
                        Composite Function [b, FunctionCall (Reference (Name "ab")) a]
                    ]
                }
            }
            let unfolded_simple_ab = unroll Recursive simple_ab
            let unfolded_on_a = FunctionCall unfolded_simple_ab a
            let unfolded_on_b = FunctionCall unfolded_simple_ab b
            -- print $ inner_expr simple_ab
            simplify unfolded_on_b `shouldBe` Valid unfolded_on_b
            simplify unfolded_on_a `shouldBe` Valid unfolded_on_a
            validateCategory unfolded_on_a `shouldBe` Valid unfolded_on_a
            validateCategory unfolded_on_b `shouldBe` Valid unfolded_on_b
            result <- runErrorableT $ execute (FunctionCall unfolded_simple_ab a)
            result `shouldBe` Valid b
            result <- runErrorableT $ execute (FunctionCall unfolded_simple_ab b)
            result `shouldBe` Valid b
        it "(Definition) should properly handle definitions" $ do
            let c = Composite Function [Definition (Placeholder (Name "x") Label (Thing (Name "5"))), Reference (Name "x")]
            result <- runErrorableT $ execute c
            result `shouldBe` Valid (Thing (Name "5"))
    describe "access" $ do
        it "should handle indices on composites well" $ do
            let composite_category = Composite Tuple [Thing (Name "a"), Thing (Name "b"), Placeholder{name=Name "c", placeholder_type=Label, placeholder_category=Thing (Name "z")}, Reference{name=Name "d"}]
            evaluateAccess Access{base=composite_category, access_id=Index 0} `shouldBe` Valid (Thing (Name "a"))
            evaluateAccess Access{base=composite_category, access_id=Index 1} `shouldBe` Valid (Thing (Name "b"))
            evaluateAccess Access{base=composite_category, access_id=Name "a"} `shouldBe` Valid (Thing (Name "a"))
            evaluateAccess Access{base=composite_category, access_id=Name "b"} `shouldBe` Valid (Thing (Name "b"))
            evaluateAccess Access{base=composite_category, access_id=Name "c"} `shouldBe` Valid Placeholder{name=Name "c", placeholder_type=Label, placeholder_category=Thing (Name "z")}
            evaluateAccess Access{base=composite_category, access_id=Name "d"} `shouldBe` Valid Reference{name=Name "d"}
    describe "unroll" $ do
        it "should unroll recursive labels" $ do
            let simple_ab = Placeholder{
                name=Name "ab",
                placeholder_type=Label,
                placeholder_category=Composite {
                    composite_type=Tuple,
                    inner_categories=[
                        Thing (Name "0"),
                        Reference (Name "ab")
                    ]
                }
            }
            unroll Recursive simple_ab `shouldBe` Composite {composite_type = Tuple, inner_categories= [Thing {name = Name "0"},Placeholder {name = Name "ab", placeholder_type=Label, placeholder_category= Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "0"},Reference {name = Name "ab"}]}}]}
    describe "importCategories" $ do
        it "should import categories test1" $ do
            let test_item = Import{category_uri=Reference (Name "test.test1")}
            result <- runErrorableT $ evaluateImport test_item
            result `shouldBe` Valid (Placeholder {name = Name "test1", placeholder_type = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Placeholder {name = Name "x", placeholder_type = Label, placeholder_category = Thing {name = Name "something"}},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "a", placeholder_type = Label, placeholder_category = Thing {name = Name "1"}},Placeholder {name = Name "b", placeholder_type = Label, placeholder_category = Reference {name = Name "x"}}]}]}]}})
        it "should import categories test2" $ do
            let test_item = Import{category_uri=Reference (Name "test.test2")}
            result <- runErrorableT $ evaluateImport test_item
            result `shouldBe` Valid Placeholder {name = Name "test2", placeholder_type = Label, placeholder_category = Composite {composite_type = Case, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "first"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "second"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}}
