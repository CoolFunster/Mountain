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
  | BadUnifyKind Kind Kind
  | BadHas Type Type
  | MatchWithNonFunction Exp
  | RecursiveWithNoTypeAnnotation Exp
  | RecursiveBindNotUsed Exp
  | OccursCheck String Type
  | BadUnifyUseCount UseCount UseCount
  | BadHasUseCount UseCount UseCount
  | ConcreteTypeOrExprInInterface StructStmt
  | KindDeclarationInStruct StructStmt
  | MustOnlyHaveConcreteTypes Type
  | TypeCallOnConcreteType Type
  | UndefinedDeclarations Exp [Id]
  deriving (Show, Eq)
