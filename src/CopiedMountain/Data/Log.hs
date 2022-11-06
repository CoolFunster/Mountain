module CopiedMountain.Data.Log where

import CopiedMountain.Data.AST

data Log =
  Step Exp [Env]
  deriving (Show)