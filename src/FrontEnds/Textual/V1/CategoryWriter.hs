-- {-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryWriter where

import CategoryData

import Data.Char (toLower)
import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate, intersperse)

nameToText :: Id -> [Char]
nameToText Unnamed = ""
nameToText (Name name) = name
nameToText (Index i) = "[" ++ show i ++ "]"

categoryToText :: Category -> Text
categoryToText = pack . categoryToString

categoryToString :: Category -> [Char]
categoryToString (Thing name) = "`" ++ nameToText name
categoryToString (Composite Tuple inner) = "(" ++ intercalate "," (map categoryToString inner) ++ ")"
categoryToString (Composite Union inner) = "|" ++ intercalate "," (map categoryToString inner) ++ "|"
categoryToString (Composite Composition inner) = "*(" ++ intercalate "," (map categoryToString inner) ++ ")"
categoryToString (Composite Case inner) = "*|" ++ intercalate "," (map categoryToString inner) ++ "|"
categoryToString (Composite Function inner) = intercalate " -> " (map categoryToString inner)
categoryToString (Placeholder name Element ph_category) = nameToText name ++ "@" ++ categoryToString ph_category
categoryToString (Placeholder name Label ph_category) = nameToText name ++ ":" ++ categoryToString ph_category
categoryToString Refined {base=_base_category, predicate=_predicate} = "{" ++ categoryToString _base_category ++ " | " ++ categoryToString _predicate ++ "}"
categoryToString Special{special_type=Flexible} = "(%)"
categoryToString Special{special_type=Universal} = "Any"
categoryToString Reference{name=name} = "$" ++ nameToText name
categoryToString FunctionCall{base=bm, argument=a} = categoryToString bm ++ "[" ++ categoryToString a ++ "]"
categoryToString Access{base=bc, access_id=_id} = "(" ++ categoryToString bc ++ ")." ++ nameToText _id
categoryToString Membership{big_category=bc, small_category=sc} = "(" ++ categoryToString bc ++ ")::(" ++ categoryToString sc ++ ")"
categoryToString Import{category_uri=import_str} = "import " ++ import_str
categoryToString Definition{def_category=cat} = "define " ++ categoryToString cat