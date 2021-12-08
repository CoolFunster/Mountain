{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryWriter where

import CategoryData

import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate)

nameToText :: Id -> [Char]
nameToText Unnamed = ""
nameToText (Name name) = name
nameToText (Index i) = "[" ++ show i ++ "]"

nameToLabel :: Id -> [Char]
nameToLabel Unnamed = ""
nameToLabel other = nameToText other ++ ":"

categoryToText :: Category -> Text
categoryToText = pack . categoryToCharList

categoryToCharList :: Category -> [Char]
categoryToCharList (Thing name) = "`" ++ nameToText name
categoryToCharList (Composite Product inner) = "(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite Sum inner) = "|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList (Composite Higher inner) = "^{" ++ intercalate "," (map categoryToCharList inner) ++ "}"
categoryToCharList (Composite Composition inner) = "*(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite Sumposition inner) = "*|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList (Morphism inp out) = categoryToCharList inp ++ " -> " ++ categoryToCharList out
categoryToCharList (Placeholder name Nothing ph_category) = nameToText name ++ "@(" ++ categoryToCharList ph_category ++ ")"
categoryToCharList (Placeholder name (Just level) ph_category) = nameToText name ++ "<" ++ show level ++ ">" ++ "@(" ++ categoryToCharList ph_category ++ ")"
categoryToCharList RefinedCategory {base_category=_base_category, predicate=_predicate} = "{" ++ categoryToCharList _base_category ++ "|" ++ categoryToCharList _predicate ++ "}"
categoryToCharList Special{special_type=Flexible} = "(%)"
categoryToCharList Special{special_type=Universal} = "%Any"
categoryToCharList Reference{name=name} = "$" ++ nameToText name
categoryToCharList MorphismCall{base_morphism=bm, argument=a} = categoryToCharList bm ++ "[" ++ categoryToCharList a ++ "]"
categoryToCharList Dereference{base_category=bc, category_id=_id} = categoryToCharList bc ++ "." ++ nameToText _id
categoryToCharList Membership{big_category=bc, small_category=sc} = categoryToCharList bc ++ "::" ++ categoryToCharList sc
categoryToCharList Label{name=name, target=category} = nameToLabel name ++ categoryToCharList category