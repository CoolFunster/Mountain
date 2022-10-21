module Main where

import Mountain
import MountainParser

import Hash

import Data.Map.Strict as M ( empty, fromList, singleton )
import System.Environment

main :: IO ()
main = do
  args <- getArgs  
  let file_path = head args
  let mountain_args = tail args
  
  term_to_run <- cleanup <$> parseFile file_path
  initial_console_hash <- randHash 
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
      ("All", Literal All),
      ("console", Unique initial_console_hash (Literal $ Thing "Console")),
      ("args", Tuple $ map (Literal . String) $ mountain_args)
    ]],
    unique_hashes=M.singleton "Console" initial_console_hash
  }
  final_result <- runMountainContextT initial_env $ evaluate term_to_run
  let (term, log) = final_result
  case term of
    Right (_, _) -> return ()
    Left e -> do
      putStrLn $ "ERROR: " ++ show e ++ "\n"
      putStrLn "LOG:"
      putStrLn (prettyLog log)
