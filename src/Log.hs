module Log where

import AST

data Log =
  Step Exp [Env]
  deriving (Show)