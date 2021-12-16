{-# LANGUAGE OverloadedStrings #-}
module FrontEndsSpec.TextualSpec.V1.CategoryParserWriterSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import qualified Test.QuickCheck as Q

import CategoryData
import FrontEnds.Textual.V1.CategoryParser

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import FrontEnds.Textual.V1.CategoryWriter
    ( categoryToText, categoryToCharList )
import Text.Megaparsec.Debug (dbg)
import Test.QuickCheck (Arbitrary(arbitrary), arbitraryPrintableChar)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import System.Posix.Internals (puts)
import qualified Data.Text.IO as TextIO
import Debug.Trace
import qualified Test.QuickCheck.Arbitrary as Q

instance Q.Arbitrary SpecialCategoryType where
    arbitrary = do
        Q.oneof [
            return Flexible,
            return Universal]

instance Q.Arbitrary Id where
    arbitrary = do
        arbitrary_name <- Q.vectorOf 15 (Q.choose ('a', 'z'))
        -- arbitrary_idx <- arbitrary
        Q.oneof [
            -- return Unnamed,
            return $ Name arbitrary_name]
            -- return $ Index arbitrary_idx]

instance Q.Arbitrary CompositionType where
    arbitrary = do
        Q.oneof [
            return Product,
            return Sum,
            -- return Higher,
            return Composition,
            return Sumposition]

instance Q.Arbitrary MorphismTermType where
    arbitrary = do
        Q.oneof [
            return Import,
            return Given,
            return Definition,
            return Return]

instance Q.Arbitrary MorphismTerm where
    arbitrary = MorphismTerm <$> arbitrary <*> arbitrary

instance Q.Arbitrary CategoryLevel where
    arbitrary = do
        some_integer <- arbitrary
        Q.oneof [
            return AnyLevel,
            return $ Specific some_integer]

arbitraryListOfSize :: Int -> Q.Gen [MorphismTerm]
arbitraryListOfSize size_ = do
    head <- arbitrary
    tail <- arbitraryListOfSize (size_-1)
    case size_ of
        0 -> return []
        1 -> return [head]
        _ -> return (head:tail)

flexListOfMinSize :: Int -> Q.Gen [MorphismTerm]
flexListOfMinSize min_size = do
    let flexList = Q.sized $ \n -> Q.frequency [(1, return []), (n, (:) <$> arbitrary <*> flexList)]
    flex_cat_list <- flexList
    let min_cat_list = arbitraryListOfSize min_size
    return flex_cat_list

instance Q.Arbitrary Category where
  arbitrary = do
        arbitrary_name <- arbitrary :: Q.Gen Id
        let flexList = Q.sized $ \n -> Q.frequency [(1, (:) <$> arbitrary <*> arbitrary), (n, (:) <$> arbitrary <*> flexList)]
        Q.frequency [
            (10, Thing <$> arbitrary),
            -- (1, Morphism <$> arbitrary <*> arbitrary),
            (1, Composite <$> arbitrary <*> arbitrary),
            (1, Placeholder <$> arbitrary <*> arbitrary <*> arbitrary),
            (1, RefinedCategory <$> (Placeholder <$> arbitrary <*> arbitrary <*> arbitrary) <*> arbitrary),
            (3, Q.oneof [
                    return Special{special_type=Flexible},
                    return Special{special_type=Universal},
                    return Reference{name=arbitrary_name}]),
            (4, CategoryData.Label <$> arbitrary <*> arbitrary ),
            (1, MorphismCall <$> Q.suchThat arbitrary isMorphic <*> arbitrary),
            (1, Dereference <$> arbitrary <*> arbitrary),
            -- (2, Membership <$> arbitrary <*> arbitrary),
            (1, IntermediateMorphism <$> flexList)]



spec :: Spec
spec = do
    describe "Writer must be parseable by parser" $ do
        -- it "Parser Writer property check" $ Q.property $
        --     \x -> (categoryToText . parseCategoryString . categoryToText) x `shouldBe` categoryToText x
        it "should handle weird inputs" $ do
            let result = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "teqtbwxzcgzbzzs"}},MorphismTerm {m_type = Given, m_category = Special {special_type = Flexible}},MorphismTerm {m_type = Return, m_category = Dereference {base_category = Thing {name = Name "qaxdhkrfcwrdazf"}, category_id = Name "pbyuagseuymmttb"}}]}
            let new_result = categoryToText result
            let parsed_result = parseCategoryText new_result
            parsed_result `shouldBe` result
        it "should handle weird inputs 2" $ do
            let result = MorphismCall {base_morphism = Special {special_type = Universal}, argument = MorphismCall {base_morphism = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Dereference {base_category = Special {special_type = Universal}, category_id = Name "zhzftofcpkstfpf"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "ydwodbwscbdpzzx"}}]}, argument = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Thing {name = Name "mymsyuvyykursxj"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "lpssbqsnstpqadc"}}]}}}
            let new_result = categoryToText result
            -- TextIO.putStrLn new_result
            let parsed_result = parseCategoryText new_result
            -- print parsed_result
            categoryToText parsed_result `shouldBe` new_result
        -- it "should handle weird inputs 3" $ do
        --     let result = Dereference {base_category = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Placeholder {name = Name "emapvbsbcgyprfn", ph_level = Just 7, ph_category = Membership {big_category = Thing {name = Name "kfjqlhsboyqqfga"}, small_category = Membership {big_category = Membership {big_category = Thing {name = Name "kjajygbbhmoznfc"}, small_category = Thing {name = Name "ktpxnrhkzucgfgv"}}, small_category = MorphismCall {base_morphism = MorphismCall {base_morphism = Dereference {base_category = Thing {name = Name "tenpjizqigwqqxv"}, category_id = Name "wmcywsfccrzztwf"}, argument = Thing {name = Name "pxcztzoicxwtdve"}}, argument = Thing {name = Name "bingfiocdijcfud"}}}}}},MorphismTerm {m_type = Return, m_category = Membership {big_category = Membership {big_category = Thing {name = Name "iseitazrsveymkr"}, small_category = Thing {name = Name "hnfehxrhbddimiz"}}, small_category = Thing {name = Name "vbcetpqimysmbgh"}}}]}, category_id = Name "lcautzzqbtavgkc"}
        --     let new_result = categoryToText result
        --     -- TextIO.putStrLn new_result
        --     let parsed_result = parseCategoryString new_result
        --     -- print parsed_result
        --     parsed_result `shouldBe` result
        it "should handle weird inputs 4" $ do
            let result = CategoryData.Label {name = Name "ksozjunvzmjbgey", target = Thing {name = Name "kcisxotsnthdbzv"}}
            let new_result = categoryToText result
            -- TextIO.putStrLn new_result
            let parsed_result = parseCategoryText new_result
            -- print parsed_result
            parsed_result `shouldBe` result
        it "should handle weird inputs 5" $ do
            -- let result = CategoryData.Label {name = Name "inswrutthyaqhex", target = Composite {composition_type = Sumposition, inner = []}}
            let result = Composite {composition_type = Sumposition, inner = []}
            let new_result = categoryToText result
            -- TextIO.putStrLn new_result
            let parsed_result = parseCategoryText new_result
            -- print parsed_result
            parsed_result `shouldBe` result
        -- it "should handle weird inputs 6" $ do
        --     let result = IntermediateMorphism {chain = [MorphismTerm {m_type = Given, m_category = Morphism {input = Reference {name = Name "kmrlvvoarylqrwc"}, output = Label {name = Name "uaypeysihezhder", target = IntermediateMorphism {chain = [MorphismTerm {m_type = Definition, m_category = Thing {name = Name "hcdrkzatldgbnfk"}},MorphismTerm {m_type = Import, m_category = Thing {name = Name "ugcdgwgnfggbzbw"}}]}}}},MorphismTerm {m_type = Given, m_category = Thing {name = Name "znbribnyrngdjch"}},MorphismTerm {m_type = Definition, m_category = Thing {name = Name "xhlepgwzlsexude"}},MorphismTerm {m_type = Return, m_category = Thing {name = Name "zkmzxwqvvildkrp"}},MorphismTerm {m_type = Given, m_category = Thing {name = Name "qjxodoycbhgirit"}},MorphismTerm {m_type = Return, m_category = Label {name = Name "pptbcqothmfnkon", target = Thing {name = Name "ucpzwgmnqqzvyze"}}}]}
        --     let new_result = categoryToText result
        --     TextIO.putStrLn new_result
        --     let parsed_result = parseCategoryString new_result
        --     print parsed_result
        --     categoryToText parsed_result `shouldBe` new_result
        


            