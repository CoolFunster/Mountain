module CopiedMountain.Data.Errors where

import CopiedMountain.Data.AST

data Error =
    NotImplemented String
  | UnboundId Id
  | BadEApp Exp Exp
  | BadFunctionDef Exp Exp
  | BadCallDef Exp Exp
  | BadBind Pattern Exp
  | Generic String
  deriving (Show, Eq)