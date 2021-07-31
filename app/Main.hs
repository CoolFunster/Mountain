module Main where

import CategoryData
import CategoryCore
import MTPLExpressionData
import MTPLExpressionCore

main :: IO ()
main = do
  let a = Thing "a"
  let b = Thing "b"
  let a_b = Morphism "a->b" a b

  let expr = Call (Object a_b) (Object a)
  putStr $ show (runMTPL expr)
