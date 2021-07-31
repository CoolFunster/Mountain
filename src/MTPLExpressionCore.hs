module MTPLExpressionCore
 where

import CategoryData
import CategoryCore
import MTPLExpressionData
import Data.Foldable (find)
import Debug.Trace (trace)
import Data.Maybe (isNothing, isJust, fromJust)

{- MTPLExpression Properties and Functions -}
categoryType :: MTPLExpression -> Maybe Category
categoryType (Object category_object)
    | validCategory category_object = Just category_object
    | otherwise = Nothing
categoryType m@(MorphismChain chain_name chain) 
    | any isNothing inner_types = Nothing
    | [] <- inner_types = Nothing
    | [Just any] <- inner_types = Just any
    | otherwise = Just (mtplCurry chain_name (map fromJust inner_types))
    where 
        inner_types = map categoryType chain
categoryType (Dereference base_expr deref_label)
    | isNothing base_expr_cat = Nothing
    | otherwise = dereference (fromJust base_expr_cat) deref_label
    where
        base_expr_cat = categoryType base_expr
categoryType (Call base_expression input_arg)
    | isNothing base_expr_cat || isNothing input_arg_cat = Nothing
    | otherwise = call (fromJust base_expr_cat) (fromJust input_arg_cat)
    where
        base_expr_cat = categoryType base_expression
        input_arg_cat = categoryType input_arg
categoryType (Construction obj_name composition_type inner_expression)
    | any isNothing inner_types = Nothing
    | composition_type `elem` [Sum, Product, Higher] = Just (Composite obj_name composition_type (map fromJust inner_types))
    | composition_type == Sumposition && all (isMorphic . fromJust) inner_types = Just (Composite obj_name composition_type (map fromJust inner_types))
    | composition_type == Sumposition && otherwise = Nothing
    | composition_type == Composition && chainIsComposable (map fromJust inner_types) = Just (Composite obj_name composition_type (map fromJust inner_types))
    | composition_type == Composition = Nothing
    where
        inner_types = map categoryType inner_expression
        chainIsComposable [] = True
        chainIsComposable [any] = True
        chainIsComposable (x:[y]) = composable x y
        chainIsComposable (x:y:ys) = composable x y && chainIsComposable (y:ys)
categoryType Reference{} = Nothing

isValidExpression :: MTPLExpression -> Bool
isValidExpression any_mtpl_expression = isJust (categoryType any_mtpl_expression)

replaceExpr :: MTPLExpression -> MTPLExpression -> MTPLExpression -> MTPLExpression
replaceExpr base_expression old new
    | base_expression == old = new
    | (MorphismChain morphism_label chain) <- base_expression = MorphismChain morphism_label (sequence (sequence (map replaceExpr chain) old) new)
    | (Dereference base_expression deref_label) <- base_expression = Dereference (replaceExpr base_expression old new) deref_label
    | (Call call_base input_arg) <- base_expression = Call (replaceExpr call_base old new) (replaceExpr input_arg old new)
    | (Construction constr_label composition_type inner_expression) <- base_expression = Construction constr_label composition_type (sequence (sequence (map replaceExpr inner_expression) old) new)
    | otherwise = base_expression

asReference :: MTPLExpression -> MTPLExpression
asReference r@Reference{} = r
asReference any_other_category = Reference (exprLabel any_other_category)

simplifyIdentitiesExpr :: MTPLExpression -> MTPLExpression
simplifyIdentitiesExpr (Object category_object) = Object (simplify category_object)
simplifyIdentitiesExpr (MorphismChain label []) = simplifyIdentitiesExpr (Object valid)
simplifyIdentitiesExpr (MorphismChain label [any]) = simplifyIdentitiesExpr any
simplifyIdentitiesExpr (MorphismChain label inner) = MorphismChain label (map simplifyIdentitiesExpr inner)
simplifyIdentitiesExpr (Dereference base_expression deref_label) = Dereference (simplifyIdentitiesExpr base_expression) deref_label
simplifyIdentitiesExpr (Call base_expression input_arg) = Call (simplifyIdentitiesExpr base_expression) (simplifyIdentitiesExpr input_arg)
simplifyIdentitiesExpr (Construction obj_name composition_type inner_expression) = Construction obj_name composition_type (map simplifyIdentitiesExpr inner_expression)
simplifyIdentitiesExpr expr = expr

replaceReferences :: MTPLExpression -> MTPLExpression
replaceReferences o@Object{} = o
replaceReferences m@(MorphismChain label []) = m
replaceReferences m@(MorphismChain label [any]) = replaceReferences any
replaceReferences m@(MorphismChain label inner) = 
    MorphismChain label (replaced_inner_head:replaceHeadRefInTail)
    where
        replaced_inner = map replaceReferences inner
        replaced_inner_head = head replaced_inner
        replaced_inner_tail = tail replaced_inner
        replaceHeadRefInTail = sequence (sequence (map replaceExpr replaced_inner_tail) (asReference replaced_inner_head)) replaced_inner_head
replaceReferences (Dereference base_expr deref_label) = Dereference (replaceReferences base_expr) deref_label
replaceReferences (Call base_expr input_arg) = Call (replaceReferences base_expr) (replaceReferences input_arg)
replaceReferences (Construction obj_name composition_type inner_expression) = Construction obj_name composition_type (map replaceReferences inner_expression)
replaceReferences r@Reference{} = r

makeRecursive :: MTPLExpression -> MTPLExpression
makeRecursive m@MorphismChain{label=morph_label, chain=chain}
    | isReference (head chain) = replaceExpr (MorphismChain morph_label (tail chain)) (head chain) (RecursiveReference m)
    | otherwise = error "bad input"

compileMTPL :: MTPLExpression -> MTPLExpression
compileMTPL input_expression = do
    {- take MTPL code and swap out simple identities first -}
    let identical_expr = simplifyIdentitiesExpr input_expression
    {- replace inner references in morphism chains-}
    let no_references_expr = replaceReferences input_expression
    if not $ isValidExpression no_references_expr
        then error "Expression invalid"
        else input_expression

-- assumes is compiled
evaluateExpr :: MTPLExpression -> MTPLExpression
evaluateExpr obj@(Object category_object) = obj
evaluateExpr morphismchain@(MorphismChain morphism_label chain) = morphismchain
-- | all isObject chain = Object (curry morphism_label evaluated_chain)
-- | otherwise = morphismchain
-- where
--     curry morphism_label [obj@Object{category_object=cat_obj}] = cat_obj
--     curry morphism_label (obj@Object{category_object=cat_obj}:[obj2@Object{category_object=cat_obj2}]) = Morphism morphism_label cat_obj cat_obj2
--     curry morphism_label (obj@Object{category_object=cat_obj}:list_of_expr) = Morphism morphism_label cat_obj (curry morphism_label list_of_expr)
evaluateExpr deref@(Dereference any_expr deref_label)
    | (Object inner_category) <- evaluated_expr = 
        case dereference inner_category deref_label of
            Just some_valid_category -> Object some_valid_category
            Nothing -> error $ "Unable to find reference " ++ deref_label ++ " in " ++ show inner_category
    | c@(Construction _ _ inner_expression) <- evaluated_expr = 
        case find (\x -> exprLabel x == deref_label) inner_expression of 
            Just x -> x
            Nothing -> error $ "Unable to find reference " ++ deref_label ++ " in " ++ show c
    | evaluated_expr == any_expr = deref
    | otherwise = evaluateExpr (Dereference evaluated_expr deref_label)
    where 
        evaluated_expr = evaluateExpr any_expr
evaluateExpr call_obj@(Call base_expression input_arg)
    | (Object base_category) <- evaluated_base_expression,
      (Object input_category) <- evaluated_input_arg = Object (
          case call base_category input_category of
              Just x -> x
              Nothing -> error $ "Unable to call " ++ show evaluated_base_expression ++ " with " ++ show evaluated_input_arg
      )
    | m@(MorphismChain morph_id (morph_head:rest_of_chain)) <- evaluated_base_expression =
        let
            evaluated_head_category = evaluateExpr morph_head
            evaluated_input_category = evaluated_input_arg
        in
          if has (category_object evaluated_head_category) (category_object evaluated_input_category)
              then evaluateExpr (MorphismChain morph_id (sequence (sequence (map replaceExpr rest_of_chain) morph_head) evaluated_head_category))
              else error $ "Morphism Chain " ++ show m ++ " does not match input " ++ show evaluated_head_category
    | evaluated_base_expression == base_expression && evaluated_input_arg  == input_arg = call_obj
    | otherwise = evaluateExpr (Call evaluated_base_expression evaluated_input_arg)
    where
        evaluated_base_expression = evaluateExpr base_expression
        evaluated_input_arg = evaluateExpr input_arg
evaluateExpr construction_expr@(Construction constr_label constr_type inner_construction)
    | all isObject evaluated_inner_constructions = Object (Composite constr_label constr_type (map category_object evaluated_inner_constructions))
    | evaluated_inner_constructions == inner_construction = construction_expr
    | otherwise = evaluateExpr (Construction constr_label constr_type evaluated_inner_constructions)
    where
        evaluated_inner_constructions = map evaluateExpr inner_construction
evaluateExpr r@(Reference _) = error $ "Cannot evaluate references. Tried to evaluate " ++ show r
evaluateExpr rr@RecursiveReference{} = rr --evaluateExpr (makeRecursive inner_exp)


runMTPL :: MTPLExpression -> MTPLExpression
runMTPL input_expr = do
    let compiled_expr = compileMTPL input_expr
    evaluateExpr compiled_expr