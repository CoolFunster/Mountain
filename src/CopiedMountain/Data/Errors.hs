module CopiedMountain.Data.Errors where

import CopiedMountain.Data.AST

data Error =
    UnboundId Id
  | BadBind Pattern Exp
  deriving (Show, Eq)