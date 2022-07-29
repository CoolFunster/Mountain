-- {-# LANGUAGE OverloadedStrings #-}
module FrontEnds.AST.V1.CategoryWriter where

import FrontEnds.AST.V1.CategoryParser (basePath)
import CategoryData
import System.Directory (doesFileExist)
import Control.Monad.Trans
import Control.Monad.Except

categoryToString :: Category -> String
categoryToString = show

categoryURIToFilePath :: String -> String
categoryURIToFilePath input_str =
    let
        repl '.' = '/'
        repl c = c
    in
        map repl input_str

categoryToFile :: FilePath -> Category -> CategoryContextT IO ()
categoryToFile import_category cat = do
    let full_path = basePath ++ categoryURIToFilePath import_category ++ ".ast.mtpl"
    fileExist <- lift $ doesFileExist full_path
    if fileExist
        then throwError [Error{error_type=BadExportFileExists, error_stack=[Reference (Name import_category)]}]
        else lift $ writeFile full_path (show cat)