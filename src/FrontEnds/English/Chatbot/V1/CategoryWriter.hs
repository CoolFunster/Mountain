{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.English.Chatbot.V1.CategoryWriter where

import CategoryData

import Data.Char (toLower)
import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate, intersperse)


levelToEnglish :: CategoryLevel -> [Char]
levelToEnglish (Specific 0) = "THING"
levelToEnglish (Specific 1) = "TYPE"
levelToEnglish (Specific _) = "MODULE"
levelToEnglish Infinite = "RECURSIVE CATEGORY"
levelToEnglish AnyLevel = "CONCEPT"

categoryToStr :: Category -> [Char]
categoryToStr = unpack . categoryToText

categoryToText :: Category -> Text
categoryToText = pack . categoryToEnglish

morphismTermTypeToEnglish :: MorphismTermType -> [Char]
morphismTermTypeToEnglish Given = "GIVEN"
morphismTermTypeToEnglish Definition = "DEFINE"
morphismTermTypeToEnglish Return = "RETURN"
morphismTermTypeToEnglish Import = "IMPORT"

morphismTermToEnglish :: MorphismTerm -> [Char]
morphismTermToEnglish MorphismTerm{m_type=m_type, m_category=m@Morphism{}} = morphismTermTypeToEnglish m_type ++ " (" ++ categoryToEnglish m ++ ")"
morphismTermToEnglish MorphismTerm{m_type=m_type, m_category=m_category} =morphismTermTypeToEnglish m_type ++ " " ++ categoryToEnglish m_category
morphismTermToEnglish MorphismTermChain{} = error "no morphism chains allowed"

categoryToEnglish :: Category -> [Char]
categoryToEnglish (Thing (Name name)) = "A THING " ++ name
categoryToEnglish (Thing _) = error "bad thing name"
categoryToEnglish (Composite Product inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = intercalate ", " (init inner_list)
    let second_part = ", AND " ++ last inner_list
    first_part ++ second_part
categoryToEnglish (Composite Sum inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = intercalate ", " (init inner_list)
    let second_part = ", OR " ++ last inner_list
    first_part ++ second_part
categoryToEnglish (Composite Set inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = "A TYPE WHICH HAS " ++ intercalate ", " (init inner_list)
    let second_part = ", AND " ++ last inner_list
    first_part ++ second_part
categoryToEnglish (Composite Higher inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = "A STRUCTURE WHICH HAS " ++ intercalate ", " (init inner_list)
    let second_part = ", AND " ++ last inner_list
    first_part ++ second_part
categoryToEnglish (Composite Composition inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = "A CHAIN OF FUNCTIONS WHERE " ++ head inner_list
    let second_part = " IS GIVEN TO " ++ intercalate " IS GIVEN TO " (tail inner_list)
    first_part ++ second_part
categoryToEnglish (Composite Sumposition inner) = do
    let inner_list = map categoryToEnglish inner
    let first_part = "IF " ++ head inner_list
    let second_part = ", " ++ intercalate " ELSE IF " (init inner_list)
    let third_part = ", " ++ intercalate " ELSE " (tail inner_list)
    first_part ++ second_part ++ third_part
categoryToEnglish m@Morphism{input=i@Morphism{}, output=o@Morphism{}} = do
    let first_part = "GIVEN A FUNCTION (" ++ categoryToEnglish i
    let second_part = "), " ++ categoryToEnglish o
    first_part ++ second_part
categoryToEnglish m@Morphism{input=i@Morphism{}, output=o} = do
    let first_part = "GIVEN A FUNCTION (" ++ categoryToEnglish i
    let second_part = "), RETURN " ++ categoryToEnglish o
    first_part ++ second_part
categoryToEnglish m@Morphism{input=i, output=o@Morphism{}} = "GIVEN " ++ categoryToEnglish i ++ ", " ++ categoryToEnglish o
categoryToEnglish m@Morphism{input=i, output=o} = "GIVEN " ++ categoryToEnglish i ++ ", RETURN " ++ categoryToEnglish o
categoryToEnglish (Placeholder name ph_level ph_category) = do
    let first_part = "SOME " ++ levelToEnglish ph_level
    let name_part = case name of
            (Name name) -> " CALLED " ++ name
            _ -> ""
    let category_part = " WHICH IS " ++ categoryToEnglish ph_category
    first_part ++ name_part ++ category_part
categoryToEnglish RefinedCategory {base_category=_base_category, predicate=_predicate} = categoryToEnglish _base_category ++ " WHICH " ++ categoryToEnglish _predicate
categoryToEnglish Special{special_type=Flexible} = "SOMETHING"
categoryToEnglish Special{special_type=Universal} = "ANYTHING"
categoryToEnglish Reference{name=name} = case name of
    (Name name) -> name
    _ -> error "unsupported name for reference"
categoryToEnglish MorphismCall{base_morphism=bm, argument=a} = "GIVE " ++ categoryToEnglish a ++ " TO " ++ categoryToEnglish bm
categoryToEnglish Dereference{base_category=bc, category_id=(Name name)} = "THE " ++ name ++ " OF " ++ categoryToEnglish bc
categoryToEnglish Dereference{base_category=bc, category_id=(Index idx)} = "THE " ++ show idx ++ " PART OF " ++ categoryToEnglish bc
categoryToEnglish Dereference{category_id=Unnamed} = error "Bad dereference"
categoryToEnglish Membership{big_category=bc, small_category=sc} = "A " ++ categoryToEnglish bc ++ " WHICH IS " ++ categoryToEnglish sc
categoryToEnglish Label{name=(Name name), target=category} = "A " ++ categoryToEnglish category ++ " NAMED " ++ name 
categoryToEnglish Label{name=_, target=category} = error "UNDEFINED error, bad label name"
categoryToEnglish IntermediateMorphism{chain=chain} = intercalate ", " (map morphismTermToEnglish chain)