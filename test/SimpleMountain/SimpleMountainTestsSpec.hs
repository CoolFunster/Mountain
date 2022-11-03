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
    res <- runIO $ getTests test_dir ""
    let res' = sort res
    forM_ res' $ \relPath -> do
      parsed <- runIO $ tryParseFile (test_dir ++ relPath)
      it ("Test: " ++ relPath) $ do
        case parsed of
          Left e -> do
            error e
          Right test -> do
            res <- runMountain $ execute (Just 30) test
            case res of
              (Left e, log) -> do

                error $ show e ++ "\n\n" ++ prettyTypedMountain test ++ "\n" ++ prettyLog log
              (Right (val, env), log) -> do
                -- putStrLn $ prettyLog log
                case val of
                  Literal Unit -> do
                    val `shouldBe` val
                    toList env `shouldBe` toList defaultState
                  other -> do
                    print $ prettyTypedMountain test ++ "\n" ++ prettyLog log
                    val `shouldBe` Literal Unit
