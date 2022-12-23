module CopiedMountain.Data.Errors where

import CopiedMountain.Data.AST

data Error =
    UnboundId Id
  | BadBind Pattern Exp
  | BadBindT Pattern Type
  | DirectUniqueCreation Exp
  | ThisShouldBeUnique Exp
  | ThisShouldBeNotUnique Exp
  | BindUniqueNonUniquely
  | BadUnify Type Type
  | BadHas Type Type
  | MatchWithNonFunction Exp
  | RecursiveWithNoTypeAnnotation Exp
  | RecursiveBindNotUsed Exp
  | OccursCheck String Type
  | BadUseCount UseCount UseCount
  | BadUsage Usage Usage
  | UnhandledUsage Usage Usage
  deriving (Show, Eq)
