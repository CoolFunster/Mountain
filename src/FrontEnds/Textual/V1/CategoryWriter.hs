-- {-# LANGUAGE OverloadedStrings #-}
module FrontEnds.Textual.V1.CategoryWriter where

import Category

import Data.Char (toLower)
import Data.Text ( Text, pack, unpack )

import Debug.Trace (trace)
import Data.List (intercalate, intersperse)
import Data.Functor.Identity (Identity (runIdentity))

idToString :: Id -> String
idToString Unnamed = ""
idToString (Name name) = name

categoryToText :: Category -> Text
categoryToText = pack . categoryToString

categoryToString :: Category -> [Char]
categoryToString (Thing name) = "`" ++ idToString name
categoryToString (Composite Tuple inner) = "(" ++ intercalate "," (map categoryToString inner) ++ ")"
categoryToString (Composite Either inner) = "|" ++ intercalate "," (map categoryToString inner) ++ "|"
categoryToString (Composite Composition inner) = "*(" ++ intercalate "," (map categoryToString inner) ++ ")*"
categoryToString (Composite Match inner) = "*|" ++ intercalate "," (map categoryToString inner) ++ "|*"
categoryToString (Composite Function inner) =
    let
        innerFooToString :: [Category] -> [Char]
        innerFooToString [] = ""
        innerFooToString [something] = categoryToString something
        innerFooToString (f@(Composite Function inner):rest) = "(" ++ categoryToString f ++ ")->" ++ innerFooToString rest
        innerFooToString (head:rest) = categoryToString head ++ "->" ++ innerFooToString rest
    in
        innerFooToString inner
categoryToString (Placeholder name Variable ph_category) = idToString name ++ "@" ++ categoryToString ph_category
categoryToString (Placeholder name Label ph_category) = idToString name ++ ":" ++ categoryToString ph_category
-- categoryToString (Placeholder name Resolved ph_category) = "<" ++ idToString name ++ ">"
categoryToString (Placeholder name Resolved ph_category) = "<" ++ categoryToString ph_category ++ ">"
categoryToString Refined {base=_base_category, predicate=_predicate} = "{" ++ categoryToString _base_category ++ " | " ++ categoryToString _predicate ++ "}"
categoryToString Special{special_type=Flexible} = "?"
categoryToString Special{special_type=Any} = "Any"
categoryToString Reference{name=name} = "$" ++ idToString name
categoryToString Call{base=bm, argument=a} = categoryToString bm ++ "[" ++ categoryToString a ++ "]"
categoryToString Access{base=bc, access_type=ByLabelGroup [label]} = "(" ++ categoryToString bc ++ ")." ++ idToString label
categoryToString Access{base=bc, access_type=ByLabelGroup labels} = "(" ++ categoryToString bc ++ ").[" ++ intercalate ", " (map idToString labels)  ++ "]"
categoryToString Access{base=bc, access_type=ByIndex idx} = "(" ++ categoryToString bc ++ ").[" ++ show idx ++ "]"
categoryToString Access{base=bc, access_type=Subtractive idx} = "(" ++ categoryToString bc ++ ").-[" ++ show idx ++ "]"
categoryToString TypeAnnotation{big_category=bc, small_category=sc} = "(" ++ categoryToString bc ++ ")::(" ++ categoryToString sc ++ ")"
categoryToString Import{import_category=import_str} = "import " ++ categoryToString import_str
categoryToString Set{elements=inner} = "{" ++ intercalate "," (map categoryToString inner) ++ "}"
categoryToString Unique{inner_category=c} = "<" ++ categoryToString c ++ ">"
categoryToString Scope{statements=s} = "*{" ++ intercalate ";" (map categoryToString s) ++ "}*"
categoryToString Binding{placeholder=ph, category_to_bind=c} = categoryToString ph ++ ": " ++ categoryToString c


errorToString :: Error -> String
errorToString e@(Error e_type stack) = show e_type ++ ": " ++ errorToStringInner e ++ foldl (\state new -> state ++ "\n\t" ++ new) "" (map categoryToString stack)

errorToStringInner :: Error -> String
errorToStringInner (Error EmptyAccessBase (Access{base=b, access_type=a_id}:_)) = "There is nothing to access in the base category: " ++ categoryToString b ++ ". Attempting to access " ++ show a_id ++ "."
errorToStringInner (Error EmptyAccessBase _) = error "unhandled"
errorToStringInner (Error EmptyAccessID (Access{base=b, access_type=a_id}:_)) = "There is an unnamed Id provided in the access of: " ++ categoryToString b
errorToStringInner (Error EmptyAccessID _) = error "unhandled"
errorToStringInner (Error BadAccess (Access{base=b, access_type=a_id}:_)) = "The lookup Id " ++ show a_id ++ " doesn't seem to exist in " ++ categoryToString b
errorToStringInner (Error BadAccess _) = error "unhandled"
errorToStringInner (Error UnresolvedReference (Reference{name=n}:rest)) = "I don't know what the reference " ++ idToString n ++ " is here. Did you mean to define this reference earlier?"
errorToStringInner (Error UnresolvedReference _) = error "unhandled"
errorToStringInner (Error CallingADataCompositeCategory _) = "You cannot call a Data Category. Did you mean to make this a function?"
errorToStringInner (Error UnnamedCategory (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be left unnamed."
errorToStringInner (Error UnnamedCategory (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be left unnamed."
errorToStringInner (Error UnnamedCategory _) = error "unhandled"
-- errorToStringInner (Error IndexedNamed (Thing{}:rest)) = "Things require textual names in order to be used. They cannot be named with an index."
-- errorToStringInner (Error IndexedNamed (Placeholder{}:rest)) = "Placeholders require textual names in order to be used. They cannot be named with an index."
-- errorToStringInner (Error IndexedNamed _) = error "unhandled"
errorToStringInner (Error EmptyFunctionalComposite _) = "This is empty. Function composites should have functions inside them. Perhaps you meant for this to be a tuple or sumple?"
errorToStringInner (Error InsufficientFunctionTerms _) = "I think this function needs more arguments for it to be properly called."
errorToStringInner (Error DataInFunctionComposite _) = "Function Composites should be composed of functions, not data"
errorToStringInner (Error BadCallInMatch stack) = "None of the functions in this case statement produce a valid result. Could be arguments or a bad formulation.\n"
errorToStringInner (Error BadCall stack) = "This function call does not produce a result or is invalid. Check the arguments?\n"
errorToStringInner (Error InvalidArgument stack) = "This function call argument is not valid for the function.\n"
errorToStringInner (Error PredicateHasNonFunctionArgument _) = "Refined types must have a function which refines the data. The predicate is not a function here."
errorToStringInner (Error NonFunctioninCallBase _) = "Function call base is not a function! Can't call a data type."
errorToStringInner (Error BadRefinementPredicateInput _) = "Refinements must accept a "
-- errorToStringInner (Error AccessIndexBelowZero _) = "Cannot access a negative index"
-- errorToStringInner (Error AccessIndexOutsideRange _) = "This index does not exist in the host category"
errorToStringInner (Error BadImport (head:rest)) = "Was not able to import "
errorToStringInner (Error BadImport _) = error "unhandled"
errorToStringInner (Error BadExportFileExists (head:rest)) = "Was not able to export since file already exists. trying to export " ++ categoryToString head
errorToStringInner (Error BadExportFileExists _) = error "unhandled"
errorToStringInner (Error CannotTypecheckRawImport (head:rest)) = "At the moment I'm unable to type check a raw import statement..."
errorToStringInner (Error CannotTypecheckRawImport _) = error "unhandled"
errorToStringInner (Error BadTypeAnnotation _) = "This membership is invalid..."
errorToStringInner (Error UndefinedAccess _) = "This Access has not been implemented yet"
errorToStringInner (Error RefinementNotHandledInAccess _) = "Accesses to Refinements are not implemented yet"
errorToStringInner (Error CannotTypecheckRawDefinition _) = "This definition needs to be evaluated before it can be typechecked"
-- errorToStringInner (Error IndexAccessOnFunction _) = "Cannot index access a function call"
-- errorToStringInner _ = error "unhandled"

errorsToString :: [Error] -> String
errorsToString = foldl (\cur new -> if cur == ""
                                    then errorToString new
                                    else cur ++ ";\n" ++ errorToString new) ""

errorableToString :: Either [Error] Category -> String
errorableToString (Right category) = categoryToString category
errorableToString (Left errors) = errorsToString errors

categoryLogToString :: CategoryLog -> String
categoryLogToString Step{msg=msg, input=input} =
  show msg ++ "\n" ++ intercalate "\n" (map prettyCategoryToString input) ++ "\n ==== \n"
categoryLogToString Has{ has_msg=hasmsg, on_which=which, big=bc, small=smc} = "HAS: " ++ show hasmsg ++ "," ++ show which ++ ": \n" ++ prettyCategoryToString bc ++ "\n--\n" ++ prettyCategoryToString smc
categoryLogToString Output {} = "Output"

categoryLogsToString :: [CategoryLog] -> String
categoryLogsToString logs = intercalate "\n---\n" $ map categoryLogToString logs

applyOnLast :: (a -> a) -> [a] -> [a]
applyOnLast f [] = []
applyOnLast f [x] = [f x]
applyOnLast f (x:xs) = x : applyOnLast f xs

applyOnFirst :: (a -> a) -> [a] -> [a]
applyOnFirst f [] = []
applyOnFirst f (x : xs) = f x : xs

mapButFirst :: (a -> a) -> [a] -> [a]
mapButFirst f [] = []
mapButFirst f [x] = [x]
mapButFirst f (x:xs) = x : map f xs

mapButLast :: (a -> a) -> [a] -> [a]
mapButLast f [] = []
mapButLast f [x] = [x]
mapButLast f (x:xs) = f x : mapButLast f xs

wrapAround :: (String, String) -> [String] -> [String]
wrapAround (init_str, last_str) input_list = applyOnFirst (init_str ++) $ applyOnLast (++ last_str) input_list

incrementIndent :: [String] -> [String]
incrementIndent = map ("\t" ++)

incrementIndentButFirst :: [String] -> [String]
incrementIndentButFirst [] = []
incrementIndentButFirst [x] = [x]
incrementIndentButFirst (x:xs) = x : incrementIndent xs

joinLastFirst :: [String] -> [String] -> [String]
joinLastFirst [] x = x
joinLastFirst x [] = x
joinLastFirst s1 s2 = init s1 ++ [last s1 ++ head s2] ++ tail s2

prepareForPrintingInner :: Category -> TransformResult (Identity Category)
prepareForPrintingInner (Composite Function ((Composite Function inner):rest)) = Recurse $ return $ Composite Function (Composite Tuple [Composite Function inner] : rest)
prepareForPrintingInner other = Recurse $ return other

prepareForPrinting :: Category -> Category
prepareForPrinting cat = runIdentity $ transformAST prepareForPrintingInner cat

prettyCategoryToStringInner :: String -> Category -> [String]
prettyCategoryToStringInner indenter (Thing name) = ["#" ++ idToString name]
prettyCategoryToStringInner indenter (Composite c_type []) = do
  case c_type of
      Tuple -> ["()"]
      Either -> ["*||*"]
      Composition -> ["()"]
      Match -> ["*||*"]
      Function -> ["<EMPTY FUNCTION>"]
prettyCategoryToStringInner indenter (Composite c_type [something]) = do
  case c_type of
      Tuple -> wrapAround ("(", ")") $ prettyCategoryToStringInner indenter something
      Either -> wrapAround ("*|", "|*") $ prettyCategoryToStringInner indenter something
      Composition -> wrapAround ("(", ")") $ prettyCategoryToStringInner indenter something
      Match -> wrapAround ("*|", "|*") $ prettyCategoryToStringInner indenter something
      Function -> prettyCategoryToStringInner indenter something
prettyCategoryToStringInner indenter (Composite Function inner) =
  let
    renderTerm :: Bool -> String -> Category -> [String]
    renderTerm add_arrow keyword x = incrementIndentButFirst $ applyOnFirst (((if add_arrow then "->" ++ indenter else "") ++ keyword) ++) (prettyCategoryToStringInner indenter x)

    toTerm :: Bool -> [Category] -> [String]
    toTerm add_arrow [] = []
    toTerm add_arrow [x] = renderTerm True "return " x
    toTerm add_arrow (i@Import{}:xs) = renderTerm add_arrow "" i ++ toTerm True xs
    toTerm add_arrow (x:xs) = renderTerm add_arrow "given " x ++ toTerm True xs
  in
    toTerm False inner
prettyCategoryToStringInner indenter (Composite c_type inner) = do
    let raw_result = map (prettyCategoryToStringInner indenter) inner
    let tabbed = map incrementIndent raw_result
    let comma = mapButLast (applyOnLast (++ ",")) tabbed
    let semicolon = mapButLast (applyOnLast (++ ";")) tabbed
    case c_type of
      Tuple -> ["("] ++ concat comma ++ [")"]
      Either -> ["*|"] ++ concat comma ++ ["*|"]
      Composition -> ["("] ++ concat semicolon ++ [")"]
      Match -> ["*|"] ++ concat semicolon ++ ["|*"]
      Function -> error "Functions should not be handled here"
prettyCategoryToStringInner indenter Import{import_category=cat} = incrementIndentButFirst $ applyOnFirst ("import " ++) $ prettyCategoryToStringInner indenter cat
prettyCategoryToStringInner indenter Reference{name=name} = ["$" ++ idToString name]
prettyCategoryToStringInner indenter (Placeholder name Label ph_category) = do
  let inner_result = prettyCategoryToStringInner indenter ph_category
  case inner_result of
    [] -> []
    something -> applyOnFirst ((idToString name ++ ":") ++) something
    -- longer_list -> (idToString name ++ ":") : incrementIndent longer_list
prettyCategoryToStringInner indenter (Placeholder name Variable ph_category) = incrementIndentButFirst $ applyOnFirst ((idToString name ++ "@") ++) $ prettyCategoryToStringInner indenter ph_category
prettyCategoryToStringInner indenter (Placeholder name Resolved ph_category) = wrapAround ("<", ">") [idToString name] -- prettyCategoryToStringInner indenter ph_category
prettyCategoryToStringInner indenter r@Refined {base=_base_category, predicate=_predicate} = [categoryToString r]
prettyCategoryToStringInner indenter Special{special_type=Flexible} = ["?"]
prettyCategoryToStringInner indenter Special{special_type=Any} = ["Any"]
prettyCategoryToStringInner indenter Call{base=bm, argument=a} = do
  let pretty_bm = wrapAround ("(", ")") (prettyCategoryToStringInner indenter bm)
  let pretty_a = wrapAround (" (", ")") (prettyCategoryToStringInner indenter a)
  joinLastFirst pretty_bm pretty_a
prettyCategoryToStringInner indenter Access{base=bc, access_type=ByLabelGroup [_id]} =
  applyOnLast (++ "." ++ idToString _id) (prettyCategoryToStringInner indenter bc)
prettyCategoryToStringInner indenter Access{base=bc, access_type=ByLabelGroup many_ids} =
  applyOnLast (++ "[" ++ intercalate "," (map idToString many_ids) ++ "]") (prettyCategoryToStringInner indenter bc)
prettyCategoryToStringInner indenter Access{base=bc, access_type=ByIndex idx} =
  applyOnLast (++ "[" ++ show idx ++ "]") (prettyCategoryToStringInner indenter bc)
prettyCategoryToStringInner indenter TypeAnnotation{big_category=bc, small_category=sc} = do
  let pretty_bc = wrapAround ("(", ")") (prettyCategoryToStringInner indenter bc)
  let pretty_sm = wrapAround ("::(", ")") (prettyCategoryToStringInner indenter sc)
  joinLastFirst pretty_bc pretty_sm
prettyCategoryToStringInner indenter Set{elements=elems} = do
  let raw_result = map (prettyCategoryToStringInner indenter) elems
  let tabbed = map incrementIndent raw_result
  let comma = mapButLast (applyOnLast (++ ",")) tabbed
  ["{"] ++ concat comma ++ ["}"]
prettyCategoryToStringInner indenter Unique{inner_category=inner} = ["<"] ++ prettyCategoryToStringInner indenter inner ++ [">"]
prettyCategoryToStringInner indenter Binding{placeholder=ph, category_to_bind=c2b} = prettyCategoryToStringInner indenter ph ++ [": "] ++ prettyCategoryToStringInner indenter c2b
prettyCategoryToStringInner indenter Scope{statements=s} = do
  let raw_result = map (prettyCategoryToStringInner indenter) s
  let tabbed = map incrementIndent raw_result
  let comma = mapButLast (applyOnLast (++ ";")) tabbed
  ["*{"] ++ concat comma ++ ["}"]

prettyCategoryToString :: Category -> String
prettyCategoryToString category = intercalate "\n" (prettyCategoryToStringInner "\t" (prepareForPrinting category))

