{-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.CategoryWriter where

import CategoryData

import Data.Text (Text)

import Debug.Trace (trace)

import Data.Text (pack, unpack)
import Data.List (intercalate)

nameToText :: Id -> [Char]
nameToText Unnamed = ""
nameToText (Name name) = name
nameToText (Index i) = "[" ++ show i ++ "]"

nameToLabel :: Id -> [Char]
nameToLabel Unnamed = ""
nameToLabel other = nameToText other ++ ":"

categoryToText :: Category -> [Char]
categoryToText (Thing name) = "`" ++ nameToText name
categoryToText (Composite name Product inner) = nameToLabel name ++ "(" ++ intercalate "," (map categoryToText inner) ++ ")"
categoryToText (Composite name Sum inner) = nameToLabel name ++ "|" ++ intercalate "," (map categoryToText inner) ++ "|"
categoryToText (Composite name Higher inner) = nameToLabel name ++ "^{" ++ intercalate "," (map categoryToText inner) ++ "}"
categoryToText (Composite name Composition inner) = nameToLabel name ++ "*(" ++ intercalate "," (map categoryToText inner) ++ ")"
categoryToText (Composite name Sumposition inner) = nameToLabel name ++ "*|" ++ intercalate "," (map categoryToText inner) ++ "|"
categoryToText (Morphism name inp out) = nameToLabel name ++ categoryToText inp ++ " -> " ++ categoryToText out
categoryToText (Placeholder name Nothing ph_category) = "{" ++ nameToText name ++ "@(" ++ categoryToText ph_category ++ ")}"
categoryToText (Placeholder name (Just level) ph_category) = "{" ++ nameToText name ++ "<" ++ show level ++ ">" ++ "@(" ++ categoryToText ph_category ++ ")}"
categoryToText RefinedCategory {name=_name, base_category=p@Placeholder{}, predicate=_predicate} = nameToLabel _name ++ "{" ++ (tail . init . categoryToText) p ++ "|" ++ categoryToText _predicate ++ "}"
categoryToText RefinedCategory {name=_name, base_category=_base_category, predicate=_predicate} = nameToLabel _name ++ "{" ++ categoryToText _base_category ++ "|" ++ categoryToText _predicate ++ "}"
categoryToText Special{name=name, special_type=Flexible} = nameToLabel name ++ "(%)"
categoryToText Special{name=name, special_type=Universal} = nameToLabel name ++ "%Any"
categoryToText Special{name=name, special_type=Reference} = "$" ++ nameToText name
categoryToText MorphismCall{base_morphism=bm, argument=a} = categoryToText bm ++ "[" ++ categoryToText a ++ "]"
categoryToText Dereference{base_category=bc, category_id=_id} = categoryToText bc ++ "." ++ nameToText _id
categoryToText Membership{big_category=bc, small_category=sc} = categoryToText bc ++ "::" ++ categoryToText sc