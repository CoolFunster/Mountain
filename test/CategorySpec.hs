{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module CategorySpec (spec) where

import Data.Either
import Debug.Trace

import Test.Hspec
import Category
import FrontEnds.AST.V1.CategoryParser

-- TODO Split into respective files

nat::Category
nat = Variable{
    name=Name "Nat",
    variable_kind=Label,
    variable_category=Composite{
        composite_type=Either,
        inner_categories=[
            Thing (Name "0"),
            Composite Tuple [Thing (Name "S"), Reference (Name "Nat")]
        ]
    }
}

simpleRecursiveCat::Category
simpleRecursiveCat = Variable{
    name=Name "self",
    variable_kind=Label,
    variable_category=Composite{composite_type=Tuple,inner_categories=[
        Thing (Name "thing"),
        Reference{name=Name "self"}]
    }
}

spec :: Spec
spec = do
    let executeAST = fmap fst . runCategoryContextT . execute Options{reduce_composite=False, importer=loadAST}
    let stepEvaluate = fmap fst . runCategoryContextT . step Options{reduce_composite=False, importer=loadAST}
    let validateCategory' = fst . runCategoryContext . validateCategory
    describe "checkAST" $ do
        it "should check properly for things" $ do
            checkAST or isThing (Thing (Name "x")) `shouldBe` True
        it "should check properly for Composites" $ do
            checkAST or isThing (Composite Tuple [Thing (Name "x")]) `shouldBe` True
        it "should work for composites with multiple categories" $ do
            checkAST or (isReferenceOfName (Name "self")) Composite{composite_type=Tuple,inner_categories=[Thing (Name "thing"), Reference{name=Name "self"}]} `shouldBe` True
        it "should check properly for Variables" $ do
            checkAST or isThing (Variable Unnamed Label (Thing (Name "x"))) `shouldBe` True
        it "should check properly for Refined" $ do
            checkAST or isThing (Refined (Thing (Name "x")) valid) `shouldBe` True
    describe "isLabelOfName" $ do
        it "Should work for labels with a particular name" $ do
            isLabelOfName (Name "x") (Thing (Name "x")) `shouldBe` False
            isLabelOfName (Name "x") (Variable (Name "x") Label valid) `shouldBe` True
            isLabelOfName (Name "x") (Variable (Name "x") Element valid) `shouldBe` False
    describe "isRecursiveCategory" $ do
        it "Simple recursion should be identified" $ do
            isRecursiveCategory simpleRecursiveCat `shouldBe` True
        it "Nats should be recursive" $ do
            isRecursiveCategory nat `shouldBe` True
    -- describe "level" $ do
    --     it "(things) should have a level of zero" $ do
    --         let thing = Thing (Name "thing")
    --         level thing `shouldBe` Right (Specific 0)
    --     it "(Either categories) should have a level of 1 more than their parts" $ do
    --         let thing = Thing (Name "thing")
    --         let thing2 = Thing (Name "thing2")
    --         let higher_category = Composite Either [thing, thing2]
    --         level higher_category `shouldBe` Right (Specific 1)
    --     it "(Tuple categories) should have a level of their components" $ do
    --         let thing = Thing (Name "thing")
    --         let thing2 = Thing (Name "thing2")
    --         let product_category = Composite Tuple [thing, thing2]
    --         level product_category `shouldBe` Right (Specific 0)
    --     it "(Morphisms) should have a level of their components" $ do
    --         let thing = Thing (Name "thing")
    --         let thing2 = Thing (Name "thing2")
    --         let morphism = Composite Function [thing, thing2]
    --         level morphism `shouldBe` Right (Specific 0)
    --     it "(Recursive) recursive categories should have an appropriate level" $ do
    --         let thing = Thing (Name "thing")
    --         let recursive_cat = Variable{
    --             name=Name "self",
    --             variable_kind=Label,
    --             variable_category=Composite{composite_type=Tuple,inner_categories=[thing, Reference{name=Name "self"}]}}
    --         level recursive_cat `shouldBe` Right (Specific 1)
    describe "has" $ do
        let has' a b = getResultOf $ has a b
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
            has' thing thing `shouldBe` Right True
            has' thing thing2 `shouldBe` Right False
            has' thing2 thing `shouldBe` Right False
        it "(things) should make things have equal singular algebraic types" $ do
            let product_things = Composite Tuple [thing]
            has' thing product_things `shouldBe` Right True
            has' thing2 product_things `shouldBe` Right False
            let product_things = Composite Tuple [thing]
            has' thing product_things `shouldBe` Right True
            has' thing2 product_things `shouldBe` Right False
        it "(morphisms) properly checks simple morphisms" $ do
            let a_b = Composite Function [a, b]
            let a_c = Composite Function [a, c]
            let b_c = Composite Function [b, c]

            has' a_b a_b `shouldBe` Right True
            has' a_b a_c `shouldBe` Right False
            has' a_b b_c `shouldBe` Right False
        it "(morphisms) does not allow intermediate morphisms" $ do
            let a_b = Composite Function [a, b]
            let a_c = Composite Function [a, c]
            let b_c = Composite Function [b, c]
            let a_b_c = Composite Function [a, b_c]

            has' a_c a_b_c `shouldBe` Right False
        it "(Product) should make Tuple of things have only a Tuple of those things" $ do
            let product_things = Composite Tuple [thing, thing2]
            has' product_things thing `shouldBe` Right False
            has' product_things thing2 `shouldBe` Right False
            has' product_things thing3 `shouldBe` Right False
            has' product_things product_things `shouldBe` Right True
            has' product_things (Composite Tuple [thing, thing3]) `shouldBe` Right False
        it "(Product) should ignore products of length 1" $ do
            let product_things = Composite Tuple [thing]
            has' product_things thing `shouldBe` Right True
            has' product_things thing2 `shouldBe` Right False
            has' product_things product_things `shouldBe` Right True
            has' product_things (Composite Tuple [thing, thing2]) `shouldBe` Right False
        it "(Either) should make a sum of things have each of those things" $ do
            let sum_things = Composite Either [thing, thing2]
            has' sum_things thing `shouldBe` Right True
            has' sum_things thing2 `shouldBe` Right True
            has' sum_things thing3 `shouldBe` Right False
            has' thing sum_things `shouldBe` Right False
            has' thing2 sum_things `shouldBe` Right False
            has' thing3 sum_things `shouldBe` Right False
            has' sum_things sum_things `shouldBe` Right True
            has' sum_things (Composite Either [thing, thing3]) `shouldBe` Right False
            has' sum_things (Composite Either [thing, thing2, thing3]) `shouldBe` Right False
            has' (Composite Either [thing, thing2, thing3]) sum_things `shouldBe` Right True
        it "(Composition) should only check input output of composite morphisms" $ do
            let composite_morphism = Composite Composition [a_b, b_c]

            has' composite_morphism a_c `shouldBe` Right True
            has' a_c composite_morphism `shouldBe` Right True
        it "(Match) should pass it on to any of the interal foos" $ do
            let sumposite_morphism = Composite Match [a_b, b_c]

            has' sumposite_morphism a_b `shouldBe` Right True
            has' sumposite_morphism b_c `shouldBe` Right True
            has' sumposite_morphism a_c `shouldBe` Right False
        it "(Either) should have each of its inner categories" $ do
            let higher_category = Composite Either [a,b,a_b,b_c]

            has' higher_category a `shouldBe` Right True
            has' higher_category b `shouldBe` Right True
            has' higher_category c `shouldBe` Right False
            has' higher_category a_b `shouldBe` Right True
            has' higher_category b_c `shouldBe` Right True
            has' higher_category a_c `shouldBe` Right False
            has' higher_category d `shouldBe` Right False
            has' higher_category a_d `shouldBe` Right False
        it "(Variable) should be contained by its category" $ do
            let higher_category = Composite Either [a,b,a_b,b_c]
            let ph_hc = Variable (Name "ph-all") Element higher_category

            has' higher_category ph_hc `shouldBe` Right True
            has' ph_hc higher_category  `shouldBe` Right True
        it "(RecursiveCategory) should have each of the component elements" $ do
            nat `has'` Thing (Name "0") `shouldBe` Right True
            nat `has'` Composite Tuple [Thing (Name "S"), Thing (Name "0")] `shouldBe` Right True
            nat `has'` Composite Tuple [Composite Tuple [Thing (Name "S"),Thing (Name "0")]] `shouldBe` Right True
    describe "call" $ do
        let call' a b = getResultOf $ call a b
        it "(NonMorphism) should return Nothing" $ do
            let a = Thing (Name "a")
            -- print $ call a a
            isLeft (call' a a) `shouldBe` True
        it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")

            let a_b = Composite Function [a, b]
            call' a a_b `shouldBe` Right b
            isLeft (call' b a_b) `shouldBe` True
        it "(Composite Composition) should handle chains right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Composite Function [a, b]
            let b_c = Composite Function [b, c]
            let a_c = Composite Function [a, c]

            let composite_morphism = Composite Composition [a_b, b_c]

            call' a composite_morphism `shouldBe` Right c
            isLeft (call' b composite_morphism) `shouldBe` True
            isLeft (call' c composite_morphism) `shouldBe` True
        it "(Composite Match) should handle sums right" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let c = Thing (Name "c")

            let a_b = Composite Function [a, b]
            let b_c = Composite Function [b, c]
            let a_c = Composite Function [a, c]

            let case_function = Composite Match [a_b, b_c]

            call' a case_function `shouldBe` Right b
            call' b case_function `shouldBe` Right c
            isLeft (call' c case_function) `shouldBe` True
    describe "validateCategory" $ do
        it "(Variable) should not modify this placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Either [a,b]
            let x_elem_a_b = Variable (Name "x") Element a_b
            validateCategory' x_elem_a_b `shouldBe` return x_elem_a_b
        it "(Variable) should not modify this function call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a,b]
            let a2b_on_a = Call a2b a
            let result = validateCategory' a2b_on_a
            result `shouldBe` Right a2b_on_a
    -- describe "simplify" $ do
    --     it "(simplify) should not modify this placeholder" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a_b = Composite Either [a,b]
    --         let x_elem_a_b = Variable (Name "x") Element a_b
    --         simplify x_elem_a_b `shouldBe` Right x_elem_a_b
    --     it "(simplify) should not modify this function call" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a2b = Composite Function [a,b]
    --         let a2b_on_a = Call a2b a
    --         let result = simplify a2b_on_a
    --         result `shouldBe` Right a2b_on_a
    describe "flatten" $ do
      let a = Thing (Name "a")
      let b = Thing (Name "b")
      let c = Thing (Name "c")
      let fooab = Composite Function [a, b]
      let foobc = Composite Function [b, c]
      it "(Thing) should just return the thing" $ do
        let result = getResultOf $ flatten a
        result `shouldBe` Right a
      it "(Function) should just return the function" $ do
        let result = getResultOf $ flatten fooab
        result `shouldBe` Right fooab
      it "(Composition) should return the flattened composite" $ do
        let foo = Composite Composition [fooab, foobc]
        let result = getResultOf $ flatten foo
        result `shouldBe` Right (Composite Function [a, c])
      it "(Match) should return the flattened case" $ do
        let foo = Composite Match [fooab, foobc]
        let result = getResultOf $ flatten foo
        result `shouldBe` Right (Composite {composite_type = Function, inner_categories = [Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]},Composite {composite_type = Either, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]})
      it "(Match) should handle nested case" $ do
        let foo = Composite Match [fooab, foobc]
        let foo2 = Composite Match [fooab, foo]
        let result = getResultOf $ flatten foo2
        isRight result `shouldBe` True
        -- todo: nested unions cleanup?
    describe "execute" $ do
        it "(Thing) should just return the thing" $ do
            let a = Thing (Name "a")
            result <- executeAST a
            result `shouldBe` Right a
        it "(Composite) should just return the composite" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let ab = Composite Tuple [a,b]
            result <- executeAST ab
            result `shouldBe` Right ab
        it "(Morphism) should just return the morphism" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a, b]
            result <- executeAST a2b
            result `shouldBe` Right a2b
        it "(Variable) should just return the placeholder" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a_b = Composite Either [a,b]
            let x_elem_a_b = Variable (Name "x") Element a_b
            result <- executeAST x_elem_a_b
            result `shouldBe` Right x_elem_a_b
        it "(Call) should just call the function" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let a2b = Composite Function [a,b]
            let a2b_on_a = Call a2b a
            result <- executeAST a2b_on_a
            result `shouldBe` Right b
        it "(RecursiveCategory) should just return itself" $ do
            result <- executeAST nat
            result `shouldBe` Right nat
        it "(RecursiveCategory) should be callable in a morphism call" $ do
            let a = Thing (Name "a")
            let b = Thing (Name "b")
            let simple_ab = Variable{
                name=Name "ab",
                variable_kind=Label,
                variable_category=Composite {
                    composite_type=Match,
                    inner_categories=[
                        Composite Function [a,b],
                        Composite Function [b, Call (Reference (Name "ab")) a]
                    ]
                }
            }
            let unfolded_simple_ab = unroll Recursive simple_ab
            let unfolded_on_a = Call unfolded_simple_ab a
            let unfolded_on_b = Call unfolded_simple_ab b
            -- print $ inner_expr simple_ab
            -- simplify unfolded_on_b `shouldBe` Right unfolded_on_b
            -- simplify unfolded_on_a `shouldBe` Right unfolded_on_a
            validateCategory' unfolded_on_a `shouldBe` Right unfolded_on_a
            validateCategory' unfolded_on_b `shouldBe` Right unfolded_on_b
            result <- stepEvaluate unfolded_on_a
            result `shouldBe` Right b
            result <- stepEvaluate unfolded_on_b
            result `shouldBe`  Right (Call {base = Variable {name = Name "ab", variable_kind = Resolved, variable_category = Variable {name = Name "ab", variable_kind = Label, variable_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]},Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Call {base = Reference {name = Name "ab"}, argument = Thing {name = Name "a"}}]}]}}}, argument = Thing {name = Name "a"}})
            let extracted_result = fromRight (error "should not hit") result
            result2 <- executeAST extracted_result
            result2 `shouldBe` Right b
        it "(Import) should evaluate imports correctly" $ do
            let c = Import (Variable (Name "x") Label (Reference (Name "test")))
            result <- getResultOfT $ evaluateImport loadAST c
            result `shouldBe` Right (Variable {name = Name "x", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "test1", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test1"}}},Variable {name = Name "test2", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test2"}}}]}})
        it "(Definition) should properly handle definitions" $ do
            let c = Composite Function [Definition (Variable (Name "x") Label (Thing (Name "5"))), Reference (Name "x")]
            result <- executeAST c
            result `shouldBe` Right (Thing (Name "5"))
        it "(Import) should properly handle imports" $ do
            let c = Composite Function [Import (Variable (Name "x") Label (Reference (Name "test"))), Reference (Name "x")]
            result1 <- stepEvaluate c
            let real_result1 = fromRight (error "should not hit 1") result1
            result2 <- stepEvaluate real_result1
            let real_result2 = fromRight (error "should not hit 2") result2
            result3 <- stepEvaluate real_result2
            result3 `shouldBe` Right (Variable {name = Name "x", variable_kind = Resolved, variable_category = Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "test1", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test1"}}},Variable {name = Name "test2", variable_kind = Label, variable_category = Import {import_category = Reference {name = Name "test.test2"}}}]}})
    describe "access" $ do
        let evaluateAccess' a = getResultOf $ evaluateAccess a
        it "should handle indices on composites well" $ do
            let composite_category = Composite Tuple [Thing (Name "a"), Thing (Name "b"), Variable{name=Name "c", variable_kind=Label, variable_category=Thing (Name "z")}, Reference{name=Name "d"}]
            evaluateAccess' Access{base=composite_category, access_id=Index 0} `shouldBe` Right (Thing (Name "a"))
            evaluateAccess' Access{base=composite_category, access_id=Index 1} `shouldBe` Right (Thing (Name "b"))
            evaluateAccess' Access{base=composite_category, access_id=Name "a"} `shouldBe` Right (Thing (Name "a"))
            evaluateAccess' Access{base=composite_category, access_id=Name "b"} `shouldBe` Right (Thing (Name "b"))
            evaluateAccess' Access{base=composite_category, access_id=Name "c"} `shouldBe` Right (Thing (Name "z"))
            evaluateAccess' Access{base=composite_category, access_id=Name "d"} `shouldBe` Right Reference{name=Name "d"}
    describe "unroll" $ do
        it "should unroll recursive labels" $ do
            let simple_ab = Variable{
                name=Name "ab",
                variable_kind=Label,
                variable_category=Composite {
                    composite_type=Tuple,
                    inner_categories=[
                        Thing (Name "0"),
                        Reference (Name "ab")
                    ]
                }
            }
            unroll Recursive simple_ab `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "0"},Variable {name = Name "ab", variable_kind = Resolved, variable_category = Variable {name = Name "ab", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "0"},Reference {name = Name "ab"}]}}}]}
    describe "importCategories" $ do
        it "should import categories test1" $ do
            let test_item = Import{import_category=Access{base=Reference (Name "test"), access_id=Name "test1"}}
            result <- getResultOfT $ evaluateImport loadAST test_item
            result `shouldBe` Right (Variable {name = Name "test1", variable_kind = Label, variable_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Variable {name = Name "x", variable_kind = Label, variable_category = Thing {name = Name "something"}},Composite {composite_type = Tuple, inner_categories = [Variable {name = Name "a", variable_kind = Label, variable_category = Thing {name = Name "1"}},Variable {name = Name "b", variable_kind = Label, variable_category = Reference {name = Name "x"}}]}]}]}})
        it "should import categories test2" $ do
            let test_item = Import{import_category=Reference (Name "test.test2")}
            result <- getResultOfT $ evaluateImport loadAST test_item
            result `shouldBe` Right Variable {name = Name "test2", variable_kind = Label, variable_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "first"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "second"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}}
