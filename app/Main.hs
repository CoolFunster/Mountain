module Main where

import CopiedMountain.Data.AST
import CopiedMountain.Interpreter
import CopiedMountain.Parser
import CopiedMountain.Typechecker
import CopiedMountain.PrettyPrinter

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import System.Environment

initialState :: State
initialState = State {
    changed=False,
    options=Options{
      parser=parseFile,
      repository="/home/mpriam/git/mtpl_language/Repository",
      file_ext=".mtn"},
    env=[]
  }

main :: IO ()
main = do
  args <- getArgs  
  let file_path = head args
  fc <- readFile file_path
  let raw_parsed = parseExpr fc
  case raw_parsed of
    Left parse_error -> error parse_error
    Right parsed -> do
      let (type_checked, _) = runTI (typeInference primitives parsed)
      case type_checked of
        Left err -> error $ prettyExp parsed ++ "\n " ++ err ++ "\n"
        Right t  -> do
          let type_str = (prettyScheme (generalize Map.empty t))
          raw_eval_result <- runWith initialState (evaluate (Just 30) parsed)
          let (eval_result, log) = raw_eval_result
          case eval_result of
            Right (res, env) -> do
              putStrLn $ type_str ++ " :: " ++ prettyExp res
            Left e -> do
              putStrLn $ "ERROR: " ++ show e ++ "\n"
              putStrLn "LOG:"
              putStrLn $ show log