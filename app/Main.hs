module Main where

import CategoryData

main :: IO ()
main = do
  let a = Thing (Name "a")
  let b = Thing (Name "b")
  let a_b = Composite{composite_type=Function, inner_categories=[a,b]}

  let expr = FunctionCall{base=a_b, argument=a}
  putStr $ show expr
