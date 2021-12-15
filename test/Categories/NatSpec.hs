module Categories.NatSpec (spec) where

import Test.Hspec
import CategoryData
import CategoryCore

import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter
import Data.Maybe
import CategoryCore

spec :: Spec
spec = do
    describe "ListNat" $ do
        it "should parse nat correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/ListNat.mtpl"
            -- print category
            category `shouldBe` Label {name = Name "ListNat", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Import, m_category = Label {name = Name "List", target = Reference {name = Name "List"}}},MorphismTerm {m_type = Return, m_category = MorphismCall {base_morphism = Reference {name = Name "List"}, argument = Composite {composition_type = Product, inner = []}}}]}}
        it "should import lists and execute correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/ListNat.mtpl"
            result <- execute category
            result `shouldBe` Label {name = Name "ListNat", target = MorphismCall {base_morphism = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Special {special_type = Universal}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Sum, inner = [Label {name = Name "empty", target = Thing {name = Name "empty_list"}},Label {name = Name "nonempty", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Reference {name = Name "list_type"}},Label {name = Name "tail", target = MorphismCall {base_morphism = Label {name = Name "List", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Special {special_type = Universal}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Sum, inner = [Label {name = Name "empty", target = Thing {name = Name "empty_list"}},Label {name = Name "nonempty", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Reference {name = Name "list_type"}},Label {name = Name "tail", target = MorphismCall {base_morphism = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}]}}]}}]}}, argument = Reference {name = Name "list_type"}}}]}}]}}]}, argument = Composite {composition_type = Product, inner = []}}}
    describe "NaturalNumber" $ do
        it "should parse NaturalNumber correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/NaturalNumber.mtpl"
            category `shouldBe` Label {name = Name "NaturalNumber", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Definition, m_category = Label {name = Name "nat_data", target = Composite {composition_type = Sum, inner = [Label {name = Name "zero", target = Composite {composition_type = Product, inner = []}},Label {name = Name "nonzero", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Composite {composition_type = Product, inner = []}},Label {name = Name "rest", target = Reference {name = Name "nat_data"}}]}}]}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "increment", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "x", ph_level = Nothing, ph_category = Reference {name = Name "nat_data"}}},MorphismTerm {m_type = Return, m_category = Membership {big_category = Reference {name = Name "nat_data"}, small_category = Composite {composition_type = Product, inner = [Composite {composition_type = Product, inner = []},Reference {name = Name "x"}]}}}]}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "decrement", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "x", ph_level = Nothing, ph_category = Dereference {base_category = Reference {name = Name "nat_data"}, category_id = Name "nonzero"}}},MorphismTerm {m_type = Return, m_category = Dereference {base_category = Reference {name = Name "x"}, category_id = Index 1}}]}}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Product, inner = [Label {name = Name "Nat", target = Reference {name = Name "nat_data"}},Label {name = Name "increment", target = Reference {name = Name "increment"}},Label {name = Name "decrement", target = Reference {name = Name "decrement"}}]}}]}}
        it "should increment correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/NaturalNumber.mtpl"
            input <- execute (parseCategoryString "import natural:$NaturalNumber -> return $natural.Nat")
            print $ categoryToStr input