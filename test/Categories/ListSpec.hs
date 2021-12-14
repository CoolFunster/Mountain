module Categories.ListSpec (spec) where

import Test.Hspec
import CategoryData
import CategoryCore

import FrontEnds.Textual.V1.CategoryParser
import FrontEnds.Textual.V1.CategoryWriter
import CategoryData
import Data.Maybe
import FrontEnds.Textual.V1.CategoryWriter (categoryToStr)
import FrontEnds.Textual.V1.CategoryParser (parseCategoryString)

spec :: Spec
spec = do
    describe "list" $ do
        it "should parse list correctly" $ do
            category <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/list.mtpl"
            -- print category
            category `shouldBe` Label {name = Name "List", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "list_type", ph_level = Nothing, ph_category = Special {special_type = Universal}}},MorphismTerm {m_type = Return, m_category = Composite {composition_type = Sum, inner = [Label {name = Name "empty", target = Thing {name = Name "empty_list"}},Label {name = Name "nonempty", target = Composite {composition_type = Product, inner = [Label {name = Name "head", target = Reference {name = Name "list_type"}},Label {name = Name "tail", target = MorphismCall {base_morphism = Reference {name = Name "List"}, argument = Reference {name = Name "list_type"}}}]}}]}}]}}
        it "should have list elements" $ do
            list <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/list.mtpl"
            list_on_anything <- execute MorphismCall{base_morphism=list,argument=universal}
            fromJust (dereference (Name "empty") list_on_anything) `shouldBe` Thing (Name "empty_list")
            (list_on_anything `has` Thing (Name "empty_list")) `shouldBe` True
            list_on_anything `has` parseCategoryString "( () , `empty_list )" `shouldBe` True
            list_on_anything `has` parseCategoryString "( `anything , `empty_list )" `shouldBe` True
            list_on_anything `has` parseCategoryString "( `anything, (`1 , `empty_list))" `shouldBe` True
        it "should not have non lists" $ do
            list <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/list.mtpl"
            list_on_anything <- execute MorphismCall{base_morphism=list,argument=universal}
            list_on_anything `has` parseCategoryString "(`something_else, `anything , `empty_list )" `shouldBe` False
        it "should exclude non haves" $ do
            list <- parseCategoryFile "/home/mpriam/git/mtpl_language/src/Categories/list.mtpl"
            let one = Thing{name=(Name "1")}
            one `isValidArgumentTo` list `shouldBe` True
            list_on_one <- execute MorphismCall{base_morphism=list,argument=one}
            list_on_one `has` parseCategoryString "( `1, (`1 , `empty_list))" `shouldBe` True
            list_on_one `has` parseCategoryString "( `anything, (`1 , `empty_list))"  `shouldBe` False
            list_on_one `has` parseCategoryString "(`1, ( `1, (`wat , `empty_list)))"  `shouldBe` False