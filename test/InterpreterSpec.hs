{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# language OverloadedStrings #-}
module InterpreterSpec (spec) where

import AST
import Context
import Parser
import Interpreter as I
import PrettyPrinter
import Hash

import Data.Map.Strict as M
import Data.Either
import Test.Hspec

-- TODO Split into respective files
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

process :: Maybe Int -> Exp -> ContextT IO Exp
process steps x = do
  I.evaluate steps x

runTest :: Int -> String -> String -> IO ()
runTest steps input_str final_str = do
  case parseExpr input_str of
    Left s -> error ("on parsing input " ++ input_str ++ "\n" ++ s)
    Right exp -> do
      (raw_res, log) <- runWith initialState $ process (Just steps) exp
      case raw_res of
        Left e -> error $ prettyError e
        Right (res, state) -> do
          case parseExpr final_str of
            Left s -> error ("on parsing final " ++ final_str ++ "\n" ++ s)
            Right exp' -> do
              res `shouldBe` exp'

spec :: Spec
spec = do
    describe "Simple Expressions" $ do
      it "Should return Literals" $ runTest 1 "3" "3"
      it "Should return simple functions" $ runTest 1 "3->4" "3->4"
      it "Should evaluate function calls" $ runTest 2 "(3->4)3" "4"
      it "Should handle let statements" $ runTest 5 "let x = 4; x" "4"
      it "Should handle multiline let statements" $ do
        let test_case = 
              " let x = 4; \
                \x          "
        let expected = "4"
        runTest 5 test_case expected
      it "Should handle type annotations" $ runTest 3 "Int :: 3" "3"
      it "Should handle type def" $ runTest 4 "type Point = (Int, Int); Point :: (3,4)" "(3,4)"
      it "Should handle match" $ runTest 10 "(3 -> 4 || 1 -> 2)3" "4"
      it "Should handle recursion" $ runTest 10 "(x ~ (2 -> (x 3) || 3 -> 4)) (2)" "4"
      it "Should handle pairs" $ runTest 1 "(3,4)" "(3,4)"
      it "Should handle labels" $ runTest 1 "x:3" "x:3"
      it "Should handle modules" $ runTest 1 "<[data x = 3;]>" "<[data x = 3;]>" 
      
        

      
        