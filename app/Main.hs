module Main where

import Mountain

main :: IO ()
main = do
  let a = Literal $ Thing "a"
  let b = Literal $ Thing "a"
  let a_b = Function [a,b]

  let expr = Call a_b a
  putStr $ show expr
