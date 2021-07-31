module MTPLExpressionData
 where

import qualified CategoryData

data MTPLExpression = 
    Object {
        category_object::CategoryData.Category
    } |
    MorphismChain {
        label::CategoryData.Id,
        chain::[MTPLExpression]
    } |
    Dereference {
        base_expression::MTPLExpression,
        deref_label::CategoryData.Id
    } |
    Call {
        base_expression::MTPLExpression, 
        input_arg::MTPLExpression
    } |
    Construction {
        obj_name::CategoryData.Id,
        composition_type::CategoryData.CompositionType,
        inner_expression::[MTPLExpression]
    } |
    Reference {
        label::CategoryData.Id
    } |
    RecursiveReference {
        ref_expr::MTPLExpression
    }
    deriving (Eq)

instance Show MTPLExpression where
    show Object{category_object=obj}="Object{"++show obj++"}"
    show MorphismChain{label=label,chain=chain}="Chain{"++show label++","++show chain++"}"
    show Dereference{base_expression=base,deref_label=deref_label}="Deref{"++show deref_label++"#"++show base++"}"
    show Call{base_expression=base,input_arg=arg}="Call{"++show base++"["++show arg++"]}"
    show Construction{obj_name=name,composition_type=comp,inner_expression=inner}="Construction{"++show name++","++show comp++"," ++ show inner ++ "}"
    show Reference{label=label}="$"++show label
    show RecursiveReference{}="%SELF%"


isObject :: MTPLExpression -> Bool
isObject Object{} = True
isObject _ = False

isMorphismChain :: MTPLExpression -> Bool
isMorphismChain MorphismChain{} = True
isMorphismChain _ = False

isReference :: MTPLExpression -> Bool
isReference Reference{} = True
isReference RecursiveReference{} = True
isReference _ = False

isRecursiveReference :: MTPLExpression -> Bool
isRecursiveReference RecursiveReference{} = True
isRecursiveReference _ = False

exprLabel :: MTPLExpression -> CategoryData.Id
exprLabel Object{category_object=cat_obj} = CategoryData.name cat_obj
exprLabel MorphismChain{label=morph_id} = morph_id
exprLabel Dereference{deref_label=d_label} = d_label
exprLabel (Call base_expression input_arg) = show (exprLabel base_expression) ++ "[" ++ show (exprLabel input_arg) ++ "]"
exprLabel (Construction obj_name _ _) = obj_name
exprLabel (Reference label) = label