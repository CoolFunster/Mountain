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
idToString (Index i) = "#" ++ show i

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
errorToString e@(Error e_type stack) = show e_type ++ ": " ++ errorToStringInner e ++ foldl (\state new -> state ++ "\n\t" ++ new) "" (map categoryToString stack)

errorToStringInner :: Error -> String
errorToStringInner (Error EmptyAccessBase (Access{base=b, access_id=a_id}:rest)) = "There is nothing to access in the base category: " ++ categoryToString b ++ ". Attempting to access field " ++  idToString a_id ++ "."
errorToStringInner (Error EmptyAccessBase _) = error "unhandled"
errorToStringInner (Error EmptyAccessID (Access{base=b, access_id=a_id}:rest)) = "There is no Id provided in the access of: " ++ categoryToString b
errorToStringInner (Error EmptyAccessID _) = error "unhandled"
errorToStringInner (Error BadAccess (Access{base=b, access_id=a_id}:rest)) = "The lookup Id " ++ idToString a_id ++ " doesn't seem to exist in " ++ categoryToString b
errorToStringInner (Error BadAccess _) = error "unhandled"
errorToStringInner (Error UnresolvedReference (Reference{name=n}:rest)) = "I don't know what the reference " ++ idToString n ++ " is here. Did you mean to define this reference earlier?"
errorToStringInner (Error UnresolvedReference _) = error "unhandled"
errorToStringInner (Error CallingADataCompositeCategory _) = "You cannot call a Data Category. Did you mean to make this a function?"
errorToStringInner (Error UnnamedCategory (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be left unnamed."
errorToStringInner (Error UnnamedCategory (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be left unnamed."
errorToStringInner (Error UnnamedCategory _) = error "unhandled"
errorToStringInner (Error IndexedNamed (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be named with an index."
errorToStringInner (Error IndexedNamed (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be named with an index."
errorToStringInner (Error IndexedNamed _) = error "unhandled"
errorToStringInner (Error EmptyFunctionalComposite _) = "This is empty. Function composites should have functions inside them. Perhaps you meant for this to be a tuple or sumple?"
errorToStringInner (Error InsufficientFunctionTerms _) = "I think this function needs more arguments for it to be properly called."
errorToStringInner (Error DataInFunctionComposite _) = "Function Composites should be composed of functions, not data"
errorToStringInner (Error BadFunctionCallInCase stack) = "None of the functions in this case statement produce a valid result. Could be arguments or a bad formulation.\n"
errorToStringInner (Error BadFunctionCall stack) = "This function call does not produce a result or is invalid. Check the arguments?\n"
errorToStringInner (Error InvalidArgument stack) = "This function call argument is not valid for the function.\n"
errorToStringInner (Error PredicateHasNonFunctionArgument _) = "Refined types must have a function which refines the data. The predicate is not a function here."
errorToStringInner (Error NonFunctioninFunctionCallBase _) = "Function call base is not a function! Can't call a data type."
errorToStringInner (Error BadRefinementPredicateInput _) = "Refinements must accept a "
errorToStringInner (Error AccessIndexBelowZero _) = "Cannot access a negative index"
errorToStringInner (Error AccessIndexOutsideRange _) = "This index does not exist in the host category"
errorToStringInner (Error BadImport (head:rest)) = "Was not able to import "
errorToStringInner (Error BadImport _) = error "unhandled"
errorToStringInner (Error BadExportFileExists (head:rest)) = "Was not able to export since file already exists. trying to export " ++ categoryToString head
errorToStringInner (Error BadExportFileExists _) = error "unhandled"
errorToStringInner (Error CannotTypecheckRawImport (head:rest)) = "At the moment I'm unable to type check a raw import statement..."
errorToStringInner (Error CannotTypecheckRawImport _) = error "unhandled"
errorToStringInner (Error BadMembership _) = "This membership is invalid..."
errorToStringInner (Error UndefinedAccess _) = "This Access has not been implemented yet"
errorToStringInner (Error RefinementNotHandledInAccess _) = "Accesses to Refinements are not implemented yet"
errorToStringInner (Error CannotTypecheckRawDefinition _) = "This definition needs to be evaluated before it can be typechecked"
-- errorToStringInner _ = error "unhandled"

errorableToString :: Errorable Category -> String
errorableToString (Valid category) = categoryToString category
errorableToString (ErrorList errors) = foldl (\cur new -> if cur == ""
                                                                then errorToString new
                                                                else cur ++ ";\n" ++ errorToString new) "" errors


