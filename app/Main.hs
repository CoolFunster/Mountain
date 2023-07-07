module Main where

import AST
import Interpreter
import Parser
import Context
import Typechecker
import PrettyPrinter

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Monad.State (MonadTrans (lift))

import System.Environment

initialState :: State
initialState = State {
    changed=False,
    options=Options{
      parser=parseFile,
      repository="/home/mpriam/git/mtpl_language/Repository",
      file_ext=".mtn"},
    env=[],
    name_counter=0
  }

processExp :: (Maybe Int) -> Exp -> ContextT IO (Scheme, Exp)
processExp steps x = do
  resType <- typeInference primitives x
  resExp <- evaluate steps x
  return (resType, resExp)

main :: IO ()
main = do
  -- Parse file
  args <- getArgs  
  let file_path = head args
  fc <- readFile file_path
  let raw_parsed = parseExpr fc
  case raw_parsed of
    Left parse_error -> error parse_error
    Right parsed -> do
      raw_eval_result <- runWith initialState (processExp (Just 30) parsed)
      let (eval_result, log) = raw_eval_result
      case eval_result of
        Right ((resT, resExp), env) -> do
          putStrLn $ prettyScheme resT ++ " :: " ++ prettyExp resExp
        Left e -> do
          putStrLn $ "ERROR: " ++ prettyError e ++ "\n"
          putStrLn "LOG:"
          putStrLn $ show log