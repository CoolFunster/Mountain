module MTPLExpressionData
 where

import CategoryData

data MTPLExpression = 
    Object {
        category_object::Category
    } |
    MorphismChain {
        chain::[MTPLExpression]
    } |
    Dereference {
        base_expression::MTPLExpression, 
        category_name::Id 
    } |
    Call {
        base_expression::MTPLExpression, 
        input_arg::MTPLExpression
    } |
    Construction {
        category_name::Id,
        composition_type::CompositionType,
        inner_expression::[MTPLExpression]
    } |
    Reference {
        ref_name::Id,
        ref_level::Int
    } 
    deriving (Show, Eq)

isObject :: MTPLExpression -> Bool
isObject Object{} = True
isObject _ = False

isMorphismChain :: MTPLExpression -> Bool
isMorphismChain MorphismChain{} = True
isMorphismChain _ = False