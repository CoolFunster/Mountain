module Categories.NatSpec (spec) where

import Test.Hspec
import CategoryData
import CategoryCore

import FrontEnds.English.Chatbot.V1.CategoryWriter (categoryToStr)
-- import FrontEnds.Textual.V1.CategoryWriter (categoryToStr)
import FrontEnds.Textual.V1.CategoryParser (parseCategoryFile, parseCategoryString, loadModule)
import Data.Maybe

import CategorySampler

spec :: Spec
spec = do
    describe "BalanceSheet" $ do
        it "parses BalanceSheet" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/balance_sheet3/balance_sheet.mtpl"
            -- print category
            category `shouldBe` Label {name = Name "ListNat", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Import, m_category = Label {name = Name "List", target = Reference {name = Name "List"}}},MorphismTerm {m_type = Import, m_category = Label {name = Name "Nat", target = Reference {name = Name "NaturalNumber"}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "list2Nat", target = Composite {composition_type = Product, inner = []}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "nat2List", target = Special {special_type = Flexible}}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Product, inner = [Label {name = Name "List2Nat", target = Reference {name = Name "list2Nat"}},Label {name = Name "Nat2List", target = Reference {name = Name "nat2List"}}]}}]}}
        it "should import lists and execute correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/List2Nat.mtpl"
            result <- execute category
            result `shouldBe`  Label {name = Name "ListNat", target = Composite {composition_type = Product, inner = [Label {name = Name "List2Nat", target = Composite {composition_type = Product, inner = []}},Label {name = Name "Nat2List", target = Special {special_type = Flexible}}]}}
    describe "NaturalNumber" $ do
        it "should parse NaturalNumber correctly" $ do
            category <- loadModule "base.naturalnumber"
            category `shouldBe` Label {name = Name "NaturalNumber", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Definition, m_category = Label {name = Name "nat_data", target = Composite {composition_type = Sum, inner = [Label {name = Name "zero", target = Composite {composition_type = Product, inner = []}},Label {name = Name "nonzero", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Composite {composition_type = Product, inner = []}},Label {name = Name "rest", target = Reference {name = Name "nat_data"}}]}}]}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "increment", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "x", ph_level = AnyLevel, ph_category = Reference {name = Name "nat_data"}}},MorphismTerm {m_type = Return, m_category = Membership {big_category = Reference {name = Name "nat_data"}, small_category = Composite {composition_type = Product, inner = [Composite {composition_type = Product, inner = []},Reference {name = Name "x"}]}}}]}}},MorphismTerm {m_type = Definition, m_category = Label {name = Name "decrement", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "x", ph_level = AnyLevel, ph_category = Dereference {base_category = Reference {name = Name "nat_data"}, category_id = Name "nonzero"}}},MorphismTerm {m_type = Return, m_category = Dereference {base_category = Reference {name = Name "x"}, category_id = Index 1}}]}}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Product, inner = [Label {name = Name "Nat", target = Reference {name = Name "nat_data"}},Label {name = Name "increment", target = Reference {name = Name "increment"}},Label {name = Name "decrement", target = Reference {name = Name "decrement"}}]}}]}}
        it "should increment & decrement correctly" $ do
            category <- loadModule "base.naturalnumber"
            zero <- execute (parseCategoryString "import natural:$NaturalNumber -> return $natural.Nat.zero")
            increment <- execute (parseCategoryString "import natural:$NaturalNumber -> return $natural.increment")
            decrement <- execute (parseCategoryString "import natural:$NaturalNumber -> return $natural.decrement")
            let one = errorableCall increment zero
            categoryToStr one `shouldBe` "((),())"
            let two = errorableCall increment one
            categoryToStr two `shouldBe` "((),((),()))"
            categoryToStr (simplify $ errorableCall decrement two) `shouldBe` categoryToStr one
            categoryToStr (simplify $ errorableCall decrement one) `shouldBe` categoryToStr zero
