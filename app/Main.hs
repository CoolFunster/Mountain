module Main where

import CategoryData
import CategoryCore

main :: IO ()
main = do
  let a = Thing (Name "a")
  let b = Thing (Name "b")
  let a_b = Morphism (Name "a->b") a b

  let expr = MorphismCall a_b a
  putStr "Hello world"
