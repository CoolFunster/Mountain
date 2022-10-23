module MountainTestsSpec where

import Mountain
    ( dotPathAsDir,
      dirAsDotPath,
      stepMany,
      toList,
      unit,
      normalize,
      MountainEnv(options),
      MountainOptions(Options) )
import MountainParser
import Test.Hspec

import           Control.Monad
import           Data.Semigroup
import qualified Data.Text.IO     as Text
import           Data.List (sort)
import           Data.List.Split
import           Text.Printf
import           System.Directory


getTests :: String -> String -> IO [String]
getTests base rel = do
  let path = base ++ rel
  dirExist <- doesDirectoryExist path
  fileExist <- doesFileExist path
  if dirExist
    then do
      new_fps <- listDirectory path
      concat <$> mapM (\x -> getTests base (rel ++ "/" ++ x)) new_fps
  else if fileExist
    then do
      let res = head $ splitOn "." rel
      return [dirAsDotPath res]
  else return []


spec :: Spec
spec = do
  let base_test_dot_path = "Tests.Base.Data.Basic"
  -- let base_test_dot_path = "Tests.CurTest"
  describe ("Mountain." ++ base_test_dot_path) $ do
    let Options _ bp ext = options defaultEnv
    res <- runIO $ getTests (bp ++ dotPathAsDir base_test_dot_path) ""
    let res' = sort res
    forM_ res' $ \dotPath ->
      it (base_test_dot_path ++ dotPath) $ do
        let example = "import " ++ base_test_dot_path ++ dotPath
        let Right term = parseString example
        res <- runMountain $ stepMany 30 (normalize term)
        case res of
          (Left e, log) -> do
            error $ show e ++ "\n\n" ++ prettyLog log
          (Right (val, env), log) -> do
            putStrLn $ prettyLog log
            val `shouldBe` unit
            toList env `shouldBe` toList defaultEnv
