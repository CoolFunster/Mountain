module Errors where

import AST

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
  | BadUnifyUseCount UseCount UseCount
  | BadHasUseCount UseCount UseCount
  deriving (Show, Eq)
