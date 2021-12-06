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
categoryToCharList (Composite name Product inner) = nameToLabel name ++ "(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite name Sum inner) = nameToLabel name ++ "|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList (Composite name Higher inner) = nameToLabel name ++ "^{" ++ intercalate "," (map categoryToCharList inner) ++ "}"
categoryToCharList (Composite name Composition inner) = nameToLabel name ++ "*(" ++ intercalate "," (map categoryToCharList inner) ++ ")"
categoryToCharList (Composite name Sumposition inner) = nameToLabel name ++ "*|" ++ intercalate "," (map categoryToCharList inner) ++ "|"
categoryToCharList (Morphism name inp out) = nameToLabel name ++ categoryToCharList inp ++ " -> " ++ categoryToCharList out
categoryToCharList (Placeholder name Nothing ph_category) = nameToText name ++ "@(" ++ categoryToCharList ph_category ++ ")"
categoryToCharList (Placeholder name (Just level) ph_category) = nameToText name ++ "<" ++ show level ++ ">" ++ "@(" ++ categoryToCharList ph_category ++ ")"
categoryToCharList RefinedCategory {name=_name, base_category=_base_category, predicate=_predicate} = nameToLabel _name ++ "{" ++ categoryToCharList _base_category ++ "|" ++ categoryToCharList _predicate ++ "}"
categoryToCharList Special{name=name, special_type=Flexible} = nameToLabel name ++ "(%)"
categoryToCharList Special{name=name, special_type=Universal} = nameToLabel name ++ "%Any"
categoryToCharList Special{name=name, special_type=Reference} = "$" ++ nameToText name
categoryToCharList MorphismCall{base_morphism=bm, argument=a} = categoryToCharList bm ++ "[" ++ categoryToCharList a ++ "]"
categoryToCharList Dereference{base_category=bc, category_id=_id} = categoryToCharList bc ++ "." ++ nameToText _id
categoryToCharList Membership{big_category=bc, small_category=sc} = categoryToCharList bc ++ "::" ++ categoryToCharList sc