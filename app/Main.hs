module Main where

import Mountain
import MountainParser

import Hash

import Data.Map.Strict as M ( empty, fromList, singleton )
import System.Environment

main :: IO ()
main = do
  args <- getArgs   
  term_to_call <- parseFile (head args)
  let term_args = map (Literal . String) $ tail args
  initial_console_hash <- randHash 
  let term_to_run = Call term_to_call (Tuple [Unique initial_console_hash (Literal $ Thing "Console"), Tuple term_args])
  let initial_env = MountainEnv {
    options=Options{
      parser=parseFile,
      repository=basePath,
      file_ext=fileExt},
    environment=[M.fromList [
      ("is", Literal Is),
      ("assert", Literal Assert),
      ("assertFail", Literal AssertFail),
      ("print", Literal Print),
      ("All", Literal All)
    ]],
    unique_hashes=M.singleton "Console" initial_console_hash
  }
  final_result <- runMountainContextT initial_env $ evaluate term_to_run
  let (term, log) = final_result
  case term of
    Right (something, _) -> print $ prettyMountain something
    Left e -> error (show e ++ "\n\n" ++ (prettyLog log))
