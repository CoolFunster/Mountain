{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryWriter where

import CategoryData

import Data.Char (toLower)
import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate, intersperse)
import CategoryData

nameToText :: Id -> [Char]
nameToText Unnamed = ""
nameToText (Name name) = name
nameToText (Index i) = "[" ++ show i ++ "]"

nameToLabel :: Id -> [Char]
nameToLabel Unnamed = ""
nameToLabel other = nameToText other ++ ":"

categoryToStr :: Category -> [Char]
categoryToStr = unpack . categoryToText

categoryToText :: Category -> Text
categoryToText = pack . categoryToCharList

morphismTermToCharList :: MorphismTerm -> [Char]
morphismTermToCharList MorphismTerm{m_type=m_type, m_category=m@Morphism{}} = map toLower (show m_type) ++ " (" ++ categoryToCharList m ++ ")"
morphismTermToCharList MorphismTerm{m_type=m_type, m_category=m_category} = map toLower (show m_type) ++ " " ++ categoryToCharList m_category
morphismTermToCharList MorphismTermChain{} = error "no morphism chains allowed"

categoryToCharList :: Category -> [Char]
categoryToCharList (Thing name) = "`" ++ nameToText name
categoryToCharList (Composite Product inner) = "(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite Sum inner) = "|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList (Composite Set inner) = "{" ++ intercalate "," (map categoryToCharList inner) ++ "}"
categoryToCharList (Composite Higher inner) = "^{" ++ intercalate "," (map categoryToCharList inner) ++ "}"
categoryToCharList (Composite Composition inner) = "*(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite Sumposition inner) = "*|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList m@Morphism{input=i@Morphism{}, output=o} = "(" ++ categoryToCharList i ++ ")->" ++ categoryToCharList o
categoryToCharList m@Morphism{input=i, output=o} = categoryToCharList i ++ "->" ++ categoryToCharList o
categoryToCharList (Placeholder name AnyLevel ph_category) = nameToText name ++ "@" ++ categoryToCharList ph_category
categoryToCharList (Placeholder name (Specific level) ph_category) = nameToText name ++ "<" ++ show level ++ ">" ++ "@" ++ categoryToCharList ph_category
categoryToCharList (Placeholder name Infinite ph_category) = nameToText name ++ "<INF>" ++ "@" ++ categoryToCharList ph_category
categoryToCharList RefinedCategory {base_category=_base_category, predicate=_predicate} = "{" ++ categoryToCharList _base_category ++ " | " ++ categoryToCharList _predicate ++ "}"
categoryToCharList Special{special_type=Flexible} = "(%)"
categoryToCharList Special{special_type=Universal} = "Any"
categoryToCharList Reference{name=name} = "$" ++ nameToText name
categoryToCharList MorphismCall{base_morphism=m@Morphism{}, argument=a} = "(" ++ categoryToCharList m ++ ")[" ++ categoryToCharList a ++ "]"
categoryToCharList MorphismCall{base_morphism=bm, argument=a} = categoryToCharList bm ++ "[" ++ categoryToCharList a ++ "]"
categoryToCharList Dereference{base_category=bc, category_id=_id} = "(" ++ categoryToCharList bc ++ ")." ++ nameToText _id
categoryToCharList Membership{big_category=bc, small_category=sc} = "(" ++ categoryToCharList bc ++ ")::(" ++ categoryToCharList sc ++ ")"
categoryToCharList Label{name=name, target=category} = nameToLabel name ++ categoryToCharList category
categoryToCharList IntermediateMorphism{chain=chain} = intercalate " -> " (map morphismTermToCharList chain)