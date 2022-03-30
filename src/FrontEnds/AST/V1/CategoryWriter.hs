-- {-# LANGUAGE OverloadedStrings #-}
module FrontEnds.AST.V1.CategoryWriter where

import FrontEnds.AST.V1.CategoryParser (basePath)
import CategoryData
import System.Directory (doesFileExist)


categoryToStr :: Category -> String
categoryToStr = show

categoryURIToFilePath :: String -> String
categoryURIToFilePath input_str =
    let
        repl '.' = '/'
        repl c = c
    in
        map repl input_str

categoryToFile :: FilePath -> Category -> ErrorableT IO () 
categoryToFile import_category cat = ErrorableT $ do
    let full_path = basePath ++ categoryURIToFilePath import_category ++ ".ast.mtpl"
    fileExist <- doesFileExist full_path
    if fileExist
        then return (ErrorList [Error{error_type=BadExportFileExists, error_stack=[Reference (Name import_category)]}])
        else fmap Valid (writeFile full_path (show cat))