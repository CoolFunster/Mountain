module CopiedMountain.Data.Errors where

import CopiedMountain.Data.AST

data Error =
    UnboundId Id
  | BadBind Pattern Exp
  | UniqueInNonUniqueCtx Pattern Exp
  | ThisShouldBeUnique Exp
  | ThisShouldBeNotUnique Exp
  | BadUnify Type Type
  | BadHas Type Type
  | MatchWithNonFunction Exp
  | RecursiveWithNoTypeAnnotation Exp
  | RecursiveBindNotUsed Exp
  | OccursCheck String Type
  deriving (Show, Eq)
