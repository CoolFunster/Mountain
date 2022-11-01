module SimpleMountain.SimpleMountainTestsSpec where

import SimpleMountain.SimpleMountain
import SimpleMountain.SimpleMountainParser
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
  -- base has "/" at end
  let path = base ++ rel
  dirExist <- doesDirectoryExist path
  fileExist <- doesFileExist path
  if dirExist
    then do
      new_fps <- listDirectory path
      concat <$> mapM (\x -> getTests base (rel ++ "/" ++ x)) new_fps
  else if fileExist
    then return [rel]
  else error $ "Bad path!!! " ++ path


spec :: Spec
spec = do
  let test_dir = basePath ++ "Tests/"
  describe ("MountainTests " ++ test_dir) $ do
    res <- runIO $ getTests basePath ""
    let res' = sort res
    tests <- runIO $ mapM (\x -> parseFile (basePath ++ x)) res'
    forM_ (zip res' tests) $ \(relPath, test) ->
      it ("Test: " ++ relPath) $ do
        res <- runMountain $ execute (Just 30) test
        case res of
          (Left e, log) -> do

            error $ show e ++ "\n\n" ++ prettyMountain test ++ "\n" ++ prettyLog log
          (Right (val, env), log) -> do
            -- putStrLn $ prettyLog log
            val `shouldBe` Extern Unit
            toList env `shouldBe` toList defaultState
