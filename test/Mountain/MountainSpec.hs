{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Mountain.MountainSpec (spec) where

import Data.Either
import qualified Data.Map.Strict as M
import Debug.Trace

import Test.Hspec
import Mountain.Mountain
import Mountain.MountainParser

-- TODO Split into respective files

nat::MountainTerm
nat = Bind
        (Reference "Nat")
        (Either [
          Literal $ Thing "0",
          Tuple [
            Literal $ Thing "S",
            Reference "Nat"
          ]
        ])


simpleRecursiveCat::MountainTerm
simpleRecursiveCat =
  Bind
    (Reference "self")
    (Tuple [
      Literal $ Thing "thing",
      Reference "self"
    ])

spec :: Spec
spec = do
    let stepAndResult env x = fst <$> runMountainContextT env (step x)
    let makeEnv x = MountainEnv (dummyImporter, x)
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
    describe "isRecursive" $ do
        it "Simple recursion should be identified" $ do
            isRecursive simpleRecursiveCat `shouldBe` True
        it "Nats should be recursive" $ do
            isRecursive nat `shouldBe` True
    describe "Bind" $ do
      it "should bind things and create a scope" $ do
        let bindable = Bind (Reference "x") a
        res <- fst <$> runMountainContextT dummyEnv (step bindable)
        res `shouldBe` Right (a, MountainEnv (dummyImporter, [M.singleton "x" a]))
      it "should bind identical things" $ do
        let bindable = Bind a a
        res <- fst <$> runMountainContextT dummyEnv (step bindable)
        res `shouldBe` Right (a, dummyEnv)
      it "should bind placeholder tuples" $ do
        let bindable = Tuple [Bind (Reference "x") (Literal Wildcard), Bind (Reference "y") (Literal Wildcard)]
        let bindee = Tuple [a, b]
        -- step 1
        res <- stepAndResult dummyEnv (Bind bindable bindee)
        let Right (val, env) = res
        val `shouldBe` Tuple [
          Bind (Bind (Reference "x") (Literal Wildcard)) (Literal (Thing "a")),
          Bind (Bind (Reference "y") (Literal Wildcard)) (Literal (Thing "b"))]
        show env `shouldBe` "[]"
        -- step 2
        res <- stepAndResult env val
        let Right (val, env) = res
        val `shouldBe` Tuple [
          Literal (Thing "a"),
          Bind (Bind (Reference "y") (Literal Wildcard)) (Literal (Thing "b"))]
        toList env `shouldBe` [[("x",Literal (Thing "a"))]]
        -- step 3
        res <- stepAndResult env val
        let Right (val, env) = res
        val `shouldBe` Tuple [
          Literal (Thing "a"),
          Literal (Thing "b")]
        toList env `shouldBe` [[("x",a), ("y",b)]]
      it "should bind placeholder functions" $ do
        let bindable = Function [Reference "x", Reference "y"]
        let bindee = Function [a, b]
        result <- stepAndResult dummyEnv $ Bind bindable bindee
        let Right (val, env) = result
        val `shouldBe` Function [
          Bind (Reference "x") (Literal (Thing "a")),
          Bind (Reference "y") (Literal (Thing "b"))]
        toList env `shouldBe` []
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [
          Literal (Thing "a"),
          Bind (Reference "y") (Literal (Thing "b"))]
        toList env `shouldBe` [[("x",a)]]
        result <- stepAndResult env val
        let Right (val, env) = result
        val `shouldBe` Function [Literal (Thing "a"),Literal (Thing "b")]
        toList env `shouldBe` [[("x",a), ("y", b)]]
      it "should handle star" $ do
        let bindable = Literal Star
        let bindee = Tuple [Bind (Reference "x") a, Bind (Reference "y") b]
        let env = dummyEnv
        result <- stepAndResult env (Bind bindable bindee)
        result `shouldBe` Left (NotImplemented (Bind bindable bindee))
    describe "has" $ do
      it "(things) equal things don't have each other" $ do
          let env = dummyEnv
          result <- stepAndResult env (Has a a)
          result `shouldBe` Left (BadHas a a )
      it "(Set) set of thing has the thing" $ do
          let env = dummyEnv
          result <- stepAndResult env (Has (Set [a]) a)
          let Right (val, env) = result
          val `shouldBe` Literal (Thing "a")
      it "(Set) set of thing a does not have thing b" $ do
          let env = dummyEnv
          result <- stepAndResult env (Has (Set [a]) b)
          result `shouldBe` Left (BadHas (Set [a]) b)
      it "(Set) should have only its elements" $ do
          let env = dummyEnv
          result <- stepAndResult env (Has (Set [a,b]) b)
          let Right (val, env) = result
          val `shouldBe` Either [Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` b
          toList env `shouldBe` []
      it "(Set) should have eithers" $ do
          let env = dummyEnv
          result <- stepAndResult env (Has (Set [a,b]) (Either [a,b]))
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "a")),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "a"),Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Literal (Thing "a"),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [Literal (Thing "a"),Literal (Thing "b")]
          toList env `shouldBe` []
      it "(Set,Either) Eithers of Sets should distribute" $ do
          let env = dummyEnv
          let val = Has (Either [Set [a], Set [b]]) b
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Either [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
      it "(Set,Each) Eaches of Sets should distribute" $ do
          let env = dummyEnv
          let val = Has (Each [Set [a], Set [b]]) b
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Each [
            Has (Set [Literal (Thing "a")]) (Literal (Thing "b")),
            Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
          toList env `shouldBe` []
      it "(Tuples) should unwrap types" $ do
          let a' = Tuple [Set [a]]
          let env = dummyEnv
          result <- stepAndResult env (Has a' a)
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
          toList env `shouldBe` []
          result <- stepAndResult env val
          let Right (val, env) = result
          val `shouldBe` Literal (Thing "a")
          toList env `shouldBe` []
      it "(Tuples) should unwrap values" $ do
          let a' = Tuple [a]
          let env = dummyEnv
          result <- stepAndResult env (Has (Set [a]) a')
          let Right (val, env) = result
          val `shouldBe` Has (Set [Literal (Thing "a")]) (Literal (Thing "a"))
      it "(Functions) properly checks simple function" $ do
          let ta_b = Function [Set [a], Set [b]]
          let a_b = Function [a, b]
          let env = dummyEnv
          result <- stepAndResult env (Has ta_b a_b)
          let Right (val, env) = result
          val `shouldBe` Function [Has (Set [Literal (Thing "a")]) (Literal (Thing "a")),Has (Set [Literal (Thing "b")]) (Literal (Thing "b"))]
    --         let a_c = Composite Function [a, c]
    --         let b_c = Composite Function [b, c]

    --         has' a_b a_b `shouldBe` Right False
    --         has' (compType a_b) a_b `shouldBe` Right True
    --         has' a_b a_c `shouldBe` Right False
    --         has' (compType a_b) a_c `shouldBe` Right False
    --         has'  a_b b_c `shouldBe` Right False
    --         has' (compType a_b) b_c `shouldBe` Right False
    --     it "(function) does not allow intermediate function" $ do
    --         let a_b_c = Composite Function [a, b_c]

    --         has' (setOf a_c) a_b_c `shouldBe` Right False
    --     it "(function) should handle either args" $ do
    --         let big = Composite Function [Set [a,b], Set [b,c]]
    --         let small = Composite Function [Composite Either [a,b], Composite Either [b,c]]
    --         has' big small `shouldBe` Right True
    --     it "(Product) should make Tuple of things have only a Tuple of those things" $ do
    --         let product_things = Composite Tuple [thing, thing2]
    --         has' (setOf product_things) thing `shouldBe` Right False
    --         has' (setOf product_things) thing2 `shouldBe` Right False
    --         has' (setOf product_things) thing3 `shouldBe` Right False
    --         has' (setOf product_things) product_things `shouldBe` Right True
    --         has' (setOf product_things) (Composite Tuple [thing, thing3]) `shouldBe` Right False
    --     it "(Product) should ignore products of length 1" $ do
    --         let product_things = Composite Tuple [thing]
    --         has'' product_things thing `shouldBe` Right True
    --         has'' product_things thing2 `shouldBe` Right False
    --         has'' product_things product_things `shouldBe` Right True
    --         has'' product_things (Composite Tuple [thing, thing2]) `shouldBe` Right False
    --     it "(Product) should handle inner eithers" $ do
    --         let big = Composite Tuple [Set [a,b], Set [b,c]]
    --         let small = Composite Tuple [Composite Either [a,b], Composite Either [b,c]]
    --         has' big small `shouldBe` Right True
    --     it "(Either) should make a sum of things have each of those things" $ do
    --         let sum_things = Composite Either [thing, thing2]
    --         has' (compType sum_things) thing `shouldBe` Right True
    --         has' (compType sum_things) thing2 `shouldBe` Right True
    --         has' (compType sum_things) thing3 `shouldBe` Right False
    --         has' (setOf thing) sum_things `shouldBe` Right False
    --         has' (setOf thing2) sum_things `shouldBe` Right False
    --         has' (setOf thing3) sum_things `shouldBe` Right False
    --         has' (setOf sum_things) sum_things `shouldBe` Right True
    --         has' (Set [thing, thing2]) sum_things `shouldBe` Right True
    --         has' (compType sum_things) sum_things `shouldBe` Right False
    --         has' sum_things (Composite Either [thing, thing3]) `shouldBe` Right False
    --         has' (setOf sum_things) (Composite Either [thing, thing3]) `shouldBe` Right False
    --         has' (compType sum_things) (Composite Either [thing, thing3]) `shouldBe` Right False
    --         has' sum_things (Composite Either [thing, thing2, thing3]) `shouldBe` Right False
    --         has' (Composite Either [thing, thing2, thing3]) sum_things `shouldBe` Right False
    --         has' (Set [thing, thing2, thing3]) sum_things `shouldBe` Right True
    --         has' (compType (Composite Either [thing, thing2, thing3])) sum_things `shouldBe` Right False
    --     it "(Either) should handle eithers of sets well" $ do
    --         let c = Composite Either [Set [thing], Set [thing2]]
    --         has' c thing `shouldBe` Right True
    --         has' c thing2 `shouldBe` Right True
    --         has' c (Set [thing]) `shouldBe` Right False
    --     it "(Composition) should only check input output of composite function" $ do
    --         let composite_morphism = Composite Composition [a_b, b_c]
    --         let tcomposite_morphism = Composite Composition [compType a_b, compType b_c]
    --         has' tcomposite_morphism composite_morphism `shouldBe` Right True
    --         has' (setOf a_c) composite_morphism `shouldBe` Right True
    --         has' tcomposite_morphism a_c `shouldBe` Right True
    --     it "(Match) should pass it on to any of the interal foos" $ do
    --         let sumposite_morphism = Composite Match [a_b, b_c]
    --         let flat_type = Composite Function [Set [a,b],Set [b,c]]
    --         has' flat_type sumposite_morphism `shouldBe` Right True
    --         has' flat_type a_b `shouldBe` Right True
    --         has' flat_type b_c `shouldBe` Right True
    --         has' flat_type a_c `shouldBe` Right True

    --         let basic_type = Set [a_b, b_c]
    --         has' basic_type sumposite_morphism `shouldBe` Right True
    --         has' basic_type a_b `shouldBe` Right True
    --         has' basic_type b_c `shouldBe` Right True
    --         has' basic_type a_c `shouldBe` Right False
    --     it "(Placeholder) should be contained by its category" $ do
    --         let big_c = Set [a,b,c]
    --         let ph_c = Placeholder Unnamed Variable big_c

    --         has' big_c ph_c `shouldBe` Right True
    --         has' ph_c big_c  `shouldBe` Right False
    --     -- it "(RecursiveCategory) should have each of the component elements" $ do
    --     --     nat `has'` Thing (Name "0") `shouldBe` Right True
    --     --     nat `has'` Composite Tuple [Thing (Name "S"), Thing (Name "0")] `shouldBe` Right True
    --     --     nat `has'` Composite Tuple [Composite Tuple [Thing (Name "S"),Thing (Name "0")]] `shouldBe` Right True
    -- describe "call" $ do
    --     let call' a b = getResultOf $ call a b
    --     it "(NonMorphism) should return Nothing" $ do
    --         let a = Thing (Name "a")
    --         -- print $ call a a
    --         isLeft (call' a a) `shouldBe` True
    --     it "(SimpleMorphism) should return b if inputs equal else nothing" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")

    --         let a_b = Composite Function [a, b]
    --         call' a a_b `shouldBe` Right b
    --         isLeft (call' b a_b) `shouldBe` True
    --     it "(Composite Composition) should handle chains right" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let c = Thing (Name "c")

    --         let a_b = Composite Function [a, b]
    --         let b_c = Composite Function [b, c]
    --         let a_c = Composite Function [a, c]

    --         let composite_morphism = Composite Composition [a_b, b_c]

    --         call' a composite_morphism `shouldBe` Right c
    --         isLeft (call' b composite_morphism) `shouldBe` True
    --         isLeft (call' c composite_morphism) `shouldBe` True
    --     it "(Composite Match) should handle sums right" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let c = Thing (Name "c")

    --         let a_b = Composite Function [a, b]
    --         let b_c = Composite Function [b, c]
    --         let a_c = Composite Function [a, c]

    --         let case_function = Composite Match [a_b, b_c]

    --         call' a case_function `shouldBe` Right b
    --         call' b case_function `shouldBe` Right c
    --         isLeft (call' c case_function) `shouldBe` True
    --     it "(Flex) should call flexibles as a wildcard" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let foo = Composite Function [flex, b]
    --         call' a foo `shouldBe` Right b
    --     it "(Flex) should call flexibles and replace" $ do
    --         let a = Thing (Name "a")
    --         let foo = Composite Function [Placeholder (Name "x") Label flex, Reference (Name "x")]
    --         call' a foo `shouldBe` Right (Placeholder {name = Name "x", placeholder_kind = Resolved, placeholder_category = Thing {name = Name "a"}})
    -- describe "validateCategory" $ do
    --     it "(Placeholder) should not modify this placeholder" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a_b = Composite Either [a,b]
    --         let x_elem_a_b = Placeholder (Name "x") Variable a_b
    --         validateCategory' x_elem_a_b `shouldBe` return x_elem_a_b
    --     it "(Placeholder) should not modify this function call" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a2b = Composite Function [a,b]
    --         let a2b_on_a = Call a2b a
    --         let result = validateCategory' a2b_on_a
    --         result `shouldBe` Right a2b_on_a
    -- -- describe "simplify" $ do
    -- --     it "(simplify) should not modify this placeholder" $ do
    -- --         let a = Thing (Name "a")
    -- --         let b = Thing (Name "b")
    -- --         let a_b = Composite Either [a,b]
    -- --         let x_elem_a_b = Placeholder (Name "x") Variable a_b
    -- --         simplify x_elem_a_b `shouldBe` Right x_elem_a_b
    -- --     it "(simplify) should not modify this function call" $ do
    -- --         let a = Thing (Name "a")
    -- --         let b = Thing (Name "b")
    -- --         let a2b = Composite Function [a,b]
    -- --         let a2b_on_a = Call a2b a
    -- --         let result = simplify a2b_on_a
    -- --         result `shouldBe` Right a2b_on_a
    -- describe "flatten" $ do
    --   let a = Thing (Name "a")
    --   let b = Thing (Name "b")
    --   let c = Thing (Name "c")
    --   let fooab = Composite Function [a, b]
    --   let foobc = Composite Function [b, c]
    --   it "(Thing) should just return the thing" $ do
    --     let result = getResultOf $ flatten a
    --     result `shouldBe` Right a
    --   it "(Function) should just return the function" $ do
    --     let result = getResultOf $ flatten fooab
    --     result `shouldBe` Right fooab
    --   it "(Composition) should return the flattened composite" $ do
    --     let foo = Composite Composition [fooab, foobc]
    --     let result = getResultOf $ flatten foo
    --     result `shouldBe` Right (Composite Function [a, c])
    --   it "(Match) should return the flattened case" $ do
    --     let foo = Composite Match [fooab, foobc]
    --     let result = getResultOf $ flatten foo
    --     result `shouldBe` Right (Composite {composite_type = Function, inner_categories = [Composite {composite_type = Either, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]},Composite {composite_type = Either, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]})
    --   it "(Match) should handle nested case" $ do
    --     let foo = Composite Match [fooab, foobc]
    --     let foo2 = Composite Match [fooab, foo]
    --     let result = getResultOf $ flatten foo2
    --     isRight result `shouldBe` True
    --     -- todo: nested unions cleanup?
    -- describe "execute" $ do
    --     it "(Thing) should just return the thing" $ do
    --         let a = Thing (Name "a")
    --         result <- executeAST a
    --         result `shouldBe` Right a
    --     it "(Composite) should just return the composite" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let ab = Composite Tuple [a,b]
    --         result <- executeAST ab
    --         result `shouldBe` Right ab
    --     it "(Morphism) should just return the morphism" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a2b = Composite Function [a, b]
    --         result <- executeAST a2b
    --         result `shouldBe` Right a2b
    --     it "(Placeholder) should just return the placeholder" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a_b = Composite Either [a,b]
    --         let x_elem_a_b = Placeholder (Name "x") Variable a_b
    --         result <- executeAST x_elem_a_b
    --         result `shouldBe` Right x_elem_a_b
    --     it "(Call) should just call the function" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let a2b = Composite Function [a,b]
    --         let a2b_on_a = Call a2b a
    --         result <- executeAST a2b_on_a
    --         result `shouldBe` Right b
    --     it "(RecursiveCategory) should just return itself" $ do
    --         result <- executeAST nat
    --         result `shouldBe` Right nat
    --     it "(RecursiveCategory) should be callable in a morphism call" $ do
    --         let a = Thing (Name "a")
    --         let b = Thing (Name "b")
    --         let simple_ab = Placeholder{
    --             name=Name "ab",
    --             placeholder_kind=Label,
    --             placeholder_category=Composite {
    --                 composite_type=Match,
    --                 inner_categories=[
    --                     Composite Function [a,b],
    --                     Composite Function [b, Call (Reference (Name "ab")) a]
    --                 ]
    --             }
    --         }
    --         let unfolded_simple_ab = fromRight (error "404") (getResultOf (unroll Recursive simple_ab))
    --         let unfolded_on_a = Call unfolded_simple_ab a
    --         let unfolded_on_b = Call unfolded_simple_ab b
    --         -- print $ inner_expr simple_ab
    --         -- simplify unfolded_on_b `shouldBe` Right unfolded_on_b
    --         -- simplify unfolded_on_a `shouldBe` Right unfolded_on_a
    --         validateCategory' unfolded_on_a `shouldBe` Right unfolded_on_a
    --         validateCategory' unfolded_on_b `shouldBe` Right unfolded_on_b
    --         result <- stepEvaluate unfolded_on_a
    --         result `shouldBe` Right b
    --         result <- stepEvaluate unfolded_on_b
    --         result `shouldBe`  Right (Call {base = Placeholder {name = Name "ab", placeholder_kind = Resolved, placeholder_category = Placeholder {name = Name "ab", placeholder_kind = Label, placeholder_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]},Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Call {base = Reference {name = Name "ab"}, argument = Thing {name = Name "a"}}]}]}}}, argument = Thing {name = Name "a"}})
    --         let extracted_result = fromRight (error "should not hit") result
    --         result2 <- executeAST extracted_result
    --         result2 `shouldBe` Right b
    --     it "(Import) should evaluate imports correctly" $ do
    --         let c = Import (Placeholder (Name "x") Label (Reference (Name "test")))
    --         result <- getResultOfT $ evaluateImport loadAST c
    --         result `shouldBe` Right (Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "test.test1"}}},Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "test.test2"}}}]}})
    --     it "(Import) should properly handle imports" $ do
    --         let c = Composite Function [Import (Placeholder (Name "x") Label (Reference (Name "test"))), Reference (Name "x")]
    --         result1 <- stepEvaluate c
    --         let real_result1 = fromRight (error "should not hit 1") result1
    --         result2 <- stepEvaluate real_result1
    --         let real_result2 = fromRight (error "should not hit 2") result2
    --         result3 <- stepEvaluate real_result2
    --         result3 `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "test.test1"}}},Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Import {import_category = Reference {name = Name "test.test2"}}}]})
    -- describe "access" $ do
    --     let evaluateAccess' a = getResultOf $ evaluateAccess a
    --     it "should handle indices on composites well" $ do
    --         let composite_category = Composite Tuple [Thing (Name "a"), Thing (Name "b"), Placeholder{name=Name "c", placeholder_kind=Label, placeholder_category=Thing (Name "z")}, Reference{name=Name "d"}]
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "a"]} `shouldBe` Right (Thing (Name "a"))
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "b"]} `shouldBe` Right (Thing (Name "b"))
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "c"]} `shouldBe` Right (Placeholder {name = Name "c", placeholder_kind = Label, placeholder_category = Thing {name = Name "z"}})
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "d"]} `shouldBe` Right Reference{name=Name "d"}
    --     it "should handle access on group" $ do
    --         let composite_category = Composite Tuple [
    --               Placeholder{name=Name "x", placeholder_kind=Label, placeholder_category=Thing (Name "a")},
    --               Placeholder{name=Name "y", placeholder_kind=Label, placeholder_category=Thing (Name "b")},
    --               Placeholder{name=Name "z", placeholder_kind=Label, placeholder_category=Thing (Name "c")}]
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "x", Name "y"]} `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}}]})
    --         evaluateAccess' Access{base=composite_category, access_type=ByLabelGroup [Name "x", Name "y", Name "z"]} `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}},Placeholder {name = Name "z", placeholder_kind = Label, placeholder_category = Thing {name = Name "c"}}]})
    --     it "should handle access on subtractive" $ do
    --         let composite_category = Composite Tuple [
    --               Placeholder{name=Name "x", placeholder_kind=Label, placeholder_category=Thing (Name "a")},
    --               Placeholder{name=Name "y", placeholder_kind=Label, placeholder_category=Thing (Name "b")},
    --               Placeholder{name=Name "z", placeholder_kind=Label, placeholder_category=Thing (Name "c")}]
    --         evaluateAccess' Access{base=composite_category, access_type=Subtractive [Name "x"]} `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "y", placeholder_kind = Label, placeholder_category = Thing {name = Name "b"}},Placeholder {name = Name "z", placeholder_kind = Label, placeholder_category = Thing {name = Name "c"}}]})
    --         evaluateAccess' Access{base=composite_category, access_type=Subtractive [Name "x", Name "y"]} `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "z", placeholder_kind = Label, placeholder_category = Thing {name = Name "c"}}]})
    --         evaluateAccess' Access{base=composite_category, access_type=Subtractive [Name "x", Name "y", Name "z"]} `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = []})
    -- describe "unroll" $ do
    --     it "should unroll recursive labels" $ do
    --         let simple_ab = Placeholder{
    --             name=Name "ab",
    --             placeholder_kind=Label,
    --             placeholder_category=Composite {
    --                 composite_type=Tuple,
    --                 inner_categories=[
    --                     Thing (Name "0"),
    --                     Reference (Name "ab")
    --                 ]
    --             }
    --         }
    --         let result = fromRight (error "404") (getResultOf (unroll Recursive simple_ab))
    --         result `shouldBe` Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "0"},Placeholder {name = Name "ab", placeholder_kind = Resolved, placeholder_category = Placeholder {name = Name "ab", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "0"},Reference {name = Name "ab"}]}}}]}
    -- describe "importCategories" $ do
    --     it "should import categories test1" $ do
    --         let test_item = Import{import_category=Access{base=Reference (Name "test"), access_type=ByLabelGroup [Name "test1"]}}
    --         result <- getResultOfT $ evaluateImport loadAST test_item
    --         result `shouldBe` Right (Placeholder {name = Name "test1", placeholder_kind = Label, placeholder_category = Composite {composite_type = Tuple, inner_categories = [Composite {composite_type = Function, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Label, placeholder_category = Thing {name = Name "something"}},Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "a", placeholder_kind = Label, placeholder_category = Thing {name = Name "1"}},Placeholder {name = Name "b", placeholder_kind = Label, placeholder_category = Reference {name = Name "x"}}]}]}]}})
    --     it "should import categories test2" $ do
    --         let test_item = Import{import_category=Reference (Name "test.test2")}
    --         result <- getResultOfT $ evaluateImport loadAST test_item
    --         result `shouldBe` Right Placeholder {name = Name "test2", placeholder_kind = Label, placeholder_category = Composite {composite_type = Match, inner_categories = [Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "first"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "a"},Thing {name = Name "b"}]}]},Composite {composite_type = Tuple, inner_categories = [Thing {name = Name "second"},Composite {composite_type = Function, inner_categories = [Thing {name = Name "b"},Thing {name = Name "c"}]}]}]}}
    -- describe "Scope & Binding" $ do
    --   it "(Scope) should become its return if no other statements" $ do
    --     let test_item = Scope{statements=[a]}
    --     let result = getResultOf (evaluateScope test_item)
    --     result `shouldBe` Right a
    --   it "(Scope) should handle bindings" $ do
    --     let test_item = Scope{statements=[Binding (Placeholder (Name "x") Variable universal) a, Reference (Name "x")]}
    --     let result = getResultOf (evaluateScope test_item >>= fullNormalize)
    --     result `shouldBe` Right a
    --   it "(Scope) should handle multiple bindings" $ do
    --     let test_item = Scope{statements=[
    --       Binding (Placeholder (Name "x") Variable universal) a,
    --       Binding (Placeholder (Name "y") Variable universal) b,
    --       Composite Tuple [Reference (Name "x"), Reference (Name "y")]
    --     ]}
    --     let result = getResultOf (evaluateScope test_item >>= fullNormalize)
    --     result `shouldBe` Right (Composite {composite_type = Tuple, inner_categories = [Placeholder {name = Name "x", placeholder_kind = Resolved, placeholder_category = Thing {name = Name "a"}},Placeholder {name = Name "y", placeholder_kind = Resolved, placeholder_category = Thing {name = Name "b"}}]})
