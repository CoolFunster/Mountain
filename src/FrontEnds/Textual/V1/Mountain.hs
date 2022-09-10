module FrontEnds.Textual.V1.Mountain where

import Category

import FrontEnds.Textual.V1.CategoryParser
    ( loadTextual, parseCategoryString )
import FrontEnds.Textual.V1.CategoryWriter ( errorsToString, categoryLogsToString )

strict :: CategoryEvalOptions
strict = Options{reduce_composite=True, importer=loadTextual}

lazy :: CategoryEvalOptions
lazy = Options{reduce_composite=True, importer=loadTextual}

runMountainString :: CategoryEvalOptions -> String -> IO Category
runMountainString opt input_string = do
  case parseCategoryString input_string of
    Left s -> error s
    Right cat -> do
      let execute_result = execute opt cat
      result <- getResultOfT execute_result
      log <- getLogOfT execute_result
      case result of
        Left ers -> error $ errorsToString ers -- ++ "\n===\n" ++ categoryLogsToString log
        Right cat' -> return cat'
