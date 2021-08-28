module Main where

import CategoryData
import CategoryCore

main :: IO ()
main = do
  let a = Thing "a"
  let b = Thing "b"
  let a_b = Morphism "a->b" a b

  let expr = MorphismCall a_b a
  putStr $ "Hello world"
