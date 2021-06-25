module MTPLExpressionCore
 where

import CategoryData
import CategoryCore
import MTPLExpressionData

{- MTPLExpression Properties and Functions -}
categoryType :: MTPLExpression -> Category
categoryType (Object category_object) = category_object


isValidInput :: MTPLExpression -> MTPLExpression -> Bool
isValidInput (Object category_object) (Object input_arg) = isMorphism category_object && has (input category_object) input_arg
isValidInput (Object category_object) input_arg = isValidInput (Object category_object) (Object (categoryType input_arg))
isValidInput base_expression (Object input_arg) = isValidInput (Object (categoryType base_expression)) (Object input_arg)
isValidInput base_expression input_arg = isValidInput (Object (categoryType base_expression)) (Object (categoryType input_arg))

validExpr :: MTPLExpression -> Bool
validExpr (Object category) = validCategory category
validExpr (MorphismChain inner) = all validExpr inner
validExpr (Call base_expression input_arg) = validExpr base_expression && validExpr input_arg && isValidInput base_expression input_arg
validExpr (Dereference base_expression category_name) = validExpr base_expression && elem category_name (map name (inner (categoryType base_expression)))

replaceExpr :: MTPLExpression -> MTPLExpression -> MTPLExpression -> MTPLExpression
replaceExpr base_exp old new
    | base_exp == old = new
    | MorphismChain{chain=chain} <- base_exp = MorphismChain (sequence (sequence (map replaceExpr chain) old) new)
    | Call{base_expression=base_expression, input_arg=input_arg} <- base_exp = Call (replaceExpr base_expression old new) (replaceExpr input_arg old new)
    | Dereference{base_expression=base_expression, category_name=category_name} <- base_exp = Dereference (replaceExpr base_expression old new) category_name

-- replaceReferences :: MTPLExpression -> MTPLExpression
-- replaceReferences Object _ = Object
-- replaceReferences ExpressionChain{chain=head:tail} 
--     | Object{category_object=cat_obj} <- head && isPlaceholder cat_obj = replaceExpr tail (Object (Reference (name head) (level head))) head
--     | isPlaceholder head = replaceExpr tail (Object (Reference (name head) (level head))) head
-- replaceReferences FunctionCall base_morphism input_arg = FunctionCall (replaceReferences base_morphism) (replaceReferences input_arg)
--     | obj@Object{category_object=category_object} <- base_expr = replace base_expr old new