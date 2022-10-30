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
    environment=[M.fromList $ envList ++ [
      -- ("console", Unique initial_console_hash (Extern $ Thing "Console")),
      -- ("args", Tuple $ map (Extern . String) $ mountain_args)
    ]],
    unique_hashes=M.singleton "Console" initial_console_hash
  }
  res <- runMountainContextT initial_env $ evaluate term_to_run
  print res
  let (Right (console_foo, env), _) = res
  final_result <- runMountainContextT env $ evaluate (Call console_foo $ Tuple [Unique initial_console_hash (Extern $ Thing "Console"), Tuple $ map (Extern . String) $ mountain_args])
  let (term, log) = final_result
  case term of
    Right (_, _) -> return ()
    Left e -> do
      putStrLn $ "ERROR: " ++ show e ++ "\n"
      putStrLn "LOG:"
      putStrLn (prettyLog log)
