-- {-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryWriter where

import CategoryData

import Data.Char (toLower)
import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate, intersperse)

idToString :: Id -> String
idToString Unnamed = ""
idToString (Name name) = name
idToString (Index i) = "[" ++ show i ++ "]"

categoryToText :: Category -> Text
categoryToText = pack . categoryToString

categoryToString :: Category -> [Char]
categoryToString (Thing name) = "`" ++ idToString name
categoryToString (Composite Tuple inner) = "(" ++ intercalate "," (map categoryToString inner) ++ ")"
categoryToString (Composite Union inner) = "|" ++ intercalate "," (map categoryToString inner) ++ "|"
categoryToString (Composite Composition inner) = "*(" ++ intercalate "," (map categoryToString inner) ++ ")"
categoryToString (Composite Case inner) = "*|" ++ intercalate "," (map categoryToString inner) ++ "|"
categoryToString (Composite Function inner) = 
    let
        innerFooToString :: [Category] -> [Char]
        innerFooToString [] = ""
        innerFooToString [something] = categoryToString something
        innerFooToString (f@(Composite Function inner):rest) = "(" ++ categoryToString f ++ ")->" ++ innerFooToString rest
        innerFooToString (head:rest) = categoryToString head ++ "->" ++ innerFooToString rest
    in
        innerFooToString inner
categoryToString (Placeholder name Element ph_category) = idToString name ++ "@" ++ categoryToString ph_category
categoryToString (Placeholder name Label ph_category) = idToString name ++ ":" ++ categoryToString ph_category
categoryToString Refined {base=_base_category, predicate=_predicate} = "{" ++ categoryToString _base_category ++ " | " ++ categoryToString _predicate ++ "}"
categoryToString Special{special_type=Flexible} = "(%)"
categoryToString Special{special_type=Universal} = "Any"
categoryToString Reference{name=name} = "$" ++ idToString name
categoryToString FunctionCall{base=bm, argument=a} = categoryToString bm ++ "[" ++ categoryToString a ++ "]"
categoryToString Access{base=bc, access_id=_id} = "(" ++ categoryToString bc ++ ")." ++ idToString _id
categoryToString Membership{big_category=bc, small_category=sc} = "(" ++ categoryToString bc ++ ")::(" ++ categoryToString sc ++ ")"
categoryToString Import{import_category=import_str} = "import " ++ categoryToString import_str
categoryToString Definition{def_category=cat} = "define " ++ categoryToString cat


errorToString :: Error -> String
errorToString (Error EmptyAccessBase (Access{base=b, access_id=a_id}:rest)) = "There is nothing to access in the base category: " ++ categoryToString b ++ ". Attempting to access field " ++  idToString a_id ++ "."
errorToString (Error EmptyAccessID (Access{base=b, access_id=a_id}:rest)) = "There is no Id provided in the access of: " ++ categoryToString b
errorToString (Error BadAccess (Access{base=b, access_id=a_id}:rest)) = "The lookup Id " ++ idToString a_id ++ " doesn't seem to exist in " ++ categoryToString b
errorToString (Error UnresolvedReference (Reference{name=n}:rest)) = "I don't know what the reference " ++ idToString n ++ " is here. Did you mean to define this reference earlier?"
errorToString (Error CallingADataCompositeCategory _) = "You cannot call a Data Category. Did you mean to make this a function?"
errorToString (Error UnnamedCategory (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be left unnamed."
errorToString (Error UnnamedCategory (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be left unnamed."
errorToString (Error IndexedNamed (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be named with an index."
errorToString (Error IndexedNamed (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be named with an index."
errorToString (Error EmptyFunctionalComposite _) = "This is empty. Function composites should have functions inside them. Perhaps you meant for this to be a tuple or sumple?"
errorToString (Error InsufficientFunctionTerms _) = "I think this function needs more arguments for it to be properly called."
errorToString (Error DataInFunctionComposite _) = "Function Composites should be composed of functions, not data"
errorToString (Error BadFunctionCall _) = "This function call does not produce a result or is invalid. Check the arguments?"
errorToString (Error InvalidArgument _) = "This function call argument is not valid for the function."
errorToString (Error PredicateHasNonFunctionArgument _) = "Refined types must have a function which refines the data. The predicate is not a function here."
errorToString (Error NonFunctioninFunctionCallBase _) = "Function call base is not a function! Can't call a data type."
errorToString (Error BadRefinementPredicateInput _) = "Refinements must accept a "
errorToString (Error AccessIndexBelowZero _) = "Cannot access a negative index"
errorToString (Error AccessIndexOutsideRange _) = "This index does not exist in the host category"
errorToString (Error BadImport (head:rest)) = "Was not able to import " ++ categoryToString head
errorToString (Error BadExportFileExists (head:rest)) = "Was not able to export since file already exists. trying to export" ++ categoryToString head
errorToString (Error CannotTypecheckRawImport (head:rest)) = "At the moment I'm unable to type check a raw import statement..."
errorToString (Error BadMembership _) = "This membership is invalid..."
errorToString _ = error "unhandled"


