module SequentData where

import Debug.Trace (trace)

import Data.List (zip, find, groupBy, intercalate)
import Data.Maybe (fromJust) 
import qualified Data.Set as Set
import Algebra.Graph.Labelled as Algra
import qualified Algebra.Graph.Export.Dot as AlgraDot
import qualified Algebra.Graph.ToGraph as ToGraph

import qualified CategoryData
import qualified CategoryCore

type SequentTerm = CategoryData.Category

data Sequent = Seq {
    left_terms::[SequentTerm],
    right_term::SequentTerm
} deriving (Show, Eq)

sequentSolved::Sequent -> Bool
sequentSolved Seq{left_terms=left_terms,right_term=right_term} = any (CategoryCore.categoricallyEqual right_term) left_terms

data SequentGroupGoalType = AND | OR deriving (Show,Eq)
data SequentGroup = Group {
    group_type::SequentGroupGoalType,
    sequents::[Sequent]
} deriving (Show, Eq)

data SequentRuleTypes = 
    ProductLeft | 
    ProductRight |
    SumLeft |
    SumRight |
    MorphismLeft |
    MorphismRight |
    SumpositionLeft |
    SumpositionRight |
    CompositionLeft |
    CompositionRight
    deriving (Show, Eq)

data SequentRule = Rule {
    rule_type::SequentRuleTypes,
    is_left::Bool,
    term_checker::SequentTerm -> Bool,
    move_applier::Sequent -> SequentTerm -> ([Proof], SequentGroup)
}

instance Show SequentRule where
    show (Rule rule_type is_left _ _) = "SequentRule{" ++ show rule_type ++ "}"

instance Eq SequentRule where
    (==) Rule{rule_type=name1} Rule{rule_type=name2} = name1 == name2

findApplicableTerms::SequentRule->Sequent->[SequentTerm]
findApplicableTerms rule Seq{left_terms=input_left_terms,right_term=input_right_term} = if is_left rule then output_left_terms else output_right_term
    where
        isApplicableTerm = term_checker rule
        output_left_terms = filter isApplicableTerm input_left_terms
        output_right_term = filter isApplicableTerm [input_right_term]

data ProofNode = Node {
    sequent_term::SequentTerm
} deriving (Eq)

instance Show ProofNode where
    show Node{sequent_term=term} = "Node{"++show term++"}"

instance Ord ProofNode where
    compare node1 node2 = compare (show node1) (show node2)

data ProofEdgeData = Edge {
    rule::[SequentRule], -- potentially multiple rules can lead to the same result between nodes
    inner_term_id::[CategoryData.Id]
} deriving (Eq)

instance Ord ProofEdgeData where
    compare node1 node2 = compare (show node1) (show node2)

-- These instances are needed for compatibility with algebraic graphs
instance Semigroup ProofEdgeData where
    (<>) edge1@Edge{rule=rule1,inner_term_id=id1} edge2@Edge{rule=rule2,inner_term_id=id2} = if edge1 == edge2
        then edge1
        else Edge{
            rule=rule1 ++ rule2,
            inner_term_id = id1 ++ id2
        }

instance Monoid ProofEdgeData where
    mempty = Edge {rule=[],inner_term_id=[]}

instance Show ProofEdgeData where
    show Edge{rule=rule,inner_term_id=inner_id} = show rule ++ "," ++ "[" ++ intercalate "," inner_id ++ "]"
-- end instances

type Proof = Algra.Graph ProofEdgeData ProofNode

noProof :: Proof
noProof = Algra.empty

prettyPrintProofStr :: Proof -> [Char]
prettyPrintProofStr graph = 
    let
        style = AlgraDot.Style { 
            AlgraDot.graphName               = "Proof",
            AlgraDot.preamble                = [],
            AlgraDot.graphAttributes         = [],
            AlgraDot.defaultVertexAttributes = [],
            AlgraDot.defaultEdgeAttributes   = mempty,
            AlgraDot.vertexName              = show,
            AlgraDot.vertexAttributes        = const [],
            AlgraDot.edgeAttributes          = \x y -> ["label" AlgraDot.:= show (edgeLabel x y graph)] }
    in
        AlgraDot.export style graph

proveSequent :: [SequentRule] -> Sequent -> Proof
proveSequent ruleset input_sequent
    | sequentSolved input_sequent = 
        let
            term = right_term input_sequent
        in
            -- trace ("solved_single: " ++ show input_sequent ++ prettyPrintProofStr (Algra.vertex Node{sequent_term=term})) Algra.vertex Node{sequent_term=term}
            Algra.vertex Node{sequent_term=term}
    | otherwise = do
        -- first find the applicable terms and rules
        let applicable_terms = zip ruleset (sequence (map findApplicableTerms ruleset) input_sequent)
        let first_valid_rule = find (not . null . snd) applicable_terms
        case first_valid_rule of
            Nothing -> noProof
            Just (rule, possible_terms) -> do
                let chosen_term = head possible_terms
                let (addl_proof_terms, result_seq_group) = move_applier rule input_sequent chosen_term
                -- let resulting_proof_steps = trace (unlines ["up_and",show input_sequent, show rule, show result_seq_group, prettyPrintProofStr (head addl_proof_terms)]) map (proveSequent ruleset) (sequents result_seq_group)
                let resulting_proof_steps = map (proveSequent ruleset) (sequents result_seq_group)
                -- let and_term = trace (unlines ["up_and",show input_sequent, show rule, show result_seq_group, show resulting_proof_steps, prettyPrintProofStr (head addl_proof_terms)]) Algra.overlay (head addl_proof_terms) (Algra.overlays resulting_proof_steps)
                let and_term = Algra.overlay (head addl_proof_terms) (Algra.overlays resulting_proof_steps)
                let chosen_or_proof = fromJust (find (not . Algra.isEmpty . snd) (zip addl_proof_terms resulting_proof_steps))
                let or_term = overlay (fst chosen_or_proof) (snd chosen_or_proof)
                case group_type result_seq_group of
                    AND -> if (not . any Algra.isEmpty) resulting_proof_steps
                                -- then trace ("solved_and: \n" ++ show input_sequent ++ prettyPrintProofStr and_term) and_term
                                then and_term
                                -- else trace ("unsolved_and: " ++ show input_sequent) noProof
                                else noProof
                    OR -> if (not . all Algra.isEmpty) resulting_proof_steps
                                -- then trace ("solved_or: \n" ++ show input_sequent ++ prettyPrintProofStr or_term) or_term
                                then or_term
                                -- else trace ("unsolved_or: " ++ show input_sequent) noProof
                                else noProof

pruneProof :: Proof -> ProofNode -> Proof
pruneProof result_proof goal_node = 
    let
        reachable_from_solution_node = ToGraph.reachable goal_node (Algra.transpose result_proof)
    in
        Algra.induce (`elem` reachable_from_solution_node) result_proof

solveCategorySequent :: Sequent -> Proof
solveCategorySequent input_seq = pruneProof (proveSequent categorySolverRules input_seq) Node{sequent_term=right_term input_seq}

extractProgram :: Proof -> ProofNode -> Maybe CategoryData.Category
extractProgram proof_graph goal_node@Node{sequent_term=goal_category}
    | not $ Algra.hasVertex goal_node proof_graph = Nothing
    | otherwise =
        let
            goal_node_predecessors = Set.elems $ ToGraph.preSet goal_node proof_graph
            pred_edge_data = sequence (sequence (map Algra.edgeLabel goal_node_predecessors) goal_node) proof_graph
            pred_and_edge_data = zip goal_node_predecessors pred_edge_data
            _grouped_pred_and_edge_data = groupBy (\ a b -> head (rule (snd a)) == head (rule (snd b))) pred_and_edge_data
            -- the below is basically a mapping of the seq_rule to derive the goal node paired with all the associated 
            -- pred nodes and the edges from pred nodes to goal node)
            -- the type is [(seq_rule, [(proofnode, proofedge)])]
            -- this is because there may be multiple rules which derive the same term
            grouped_pred_and_edge_data_by_rule = map (\x -> ((head . rule . snd . head) x, x)) _grouped_pred_and_edge_data
        in
            if null goal_node_predecessors
                then Just goal_category
                else 
                    let
                        -- There may be multiple rules to derive a term.
                        -- we only care about one of the ways we get to a term. 
                        -- later we can handle this choice better
                        chosen_rule = (fst . head) grouped_pred_and_edge_data_by_rule
                        related_preds_and_edges = (snd . head) grouped_pred_and_edge_data_by_rule
                    in
                        case rule_type chosen_rule of
                            ProductLeft ->
                                let
                                    productLeftPred = (fst . head) related_preds_and_edges
                                    productLeftLabel = (head . inner_term_id . snd . head) related_preds_and_edges
                                in
                                    Just $ MTPLExpressionData.Dereference (fromJust (extractProgram proof_graph productLeftPred)) productLeftLabel  
                            ProductRight ->
                                let 
                                    inner_expressions = map (extractProgram proof_graph) goal_node_predecessors
                                    term_name = CategoryData.name goal_category
                                in
                                    Just $ MTPLExpressionData.Construction{
                                        obj_name=term_name,
                                        composition_type=CategoryData.Product,
                                        inner_expression=map fromJust inner_expressions
                                    }
                            SumLeft ->
                                let
                                    inner_expressions = map (extractProgram proof_graph) goal_node_predecessors
                                    substitute_nothings predecessor term = 
                                        case term of
                                            Nothing -> Object $ sequent_term predecessor
                                            Just x -> x
                                    apply_list_foo_to_list_args= zipWith ($)
                                in
                                    Just $ MTPLExpressionData.Construction{
                                        obj_name=CategoryData.name goal_category,
                                        composition_type=CategoryData.Sumposition,
                                        inner_expression=apply_list_foo_to_list_args (map substitute_nothings goal_node_predecessors) inner_expressions
                                    }
                            SumRight ->
                                let
                                    inner_solved_expr = head (map (extractProgram proof_graph) goal_node_predecessors)
                                in
                                    inner_solved_expr
                            MorphismLeft ->
                                let
                                    input_solved_expr = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "input") related_preds_and_edges)
                                    morphism_solved_expr = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "morphism") related_preds_and_edges)
                                in
                                    Just $ MTPLExpressionData.Call {
                                        base_expression=morphism_solved_expr,
                                        input_arg=input_solved_expr
                                    }
                            MorphismRight ->
                                let
                                    assumed_input = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "assumed") related_preds_and_edges)
                                    derived_output = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "to_prove") related_preds_and_edges)
                                in
                                    Just $ MTPLExpressionData.MorphismChain{
                                        label=CategoryData.name goal_category,
                                        chain=[assumed_input, derived_output]
                                    }
                            SumpositionLeft ->
                                let
                                    inner_expressions = map (fromJust . extractProgram proof_graph) goal_node_predecessors
                                in
                                    Just $ MTPLExpressionData.Construction{
                                        obj_name=CategoryData.name goal_category,
                                        composition_type=CategoryData.Sumposition,
                                        inner_expression=inner_expressions
                                    }
                            SumpositionRight ->
                                let
                                    inner_expressions = map (fromJust . extractProgram proof_graph) goal_node_predecessors
                                in
                                    Just $ MTPLExpressionData.Construction{
                                        obj_name=CategoryData.name goal_category,
                                        composition_type=CategoryData.Sumposition,
                                        inner_expression=inner_expressions
                                    }
                            CompositionLeft ->
                                let
                                    input_solved_expr = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "input") related_preds_and_edges)
                                    morphism_solved_expr = fromJust $ extractProgram proof_graph $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "morphism") related_preds_and_edges)
                                in
                                    Just $ MTPLExpressionData.Call {
                                        base_expression=morphism_solved_expr,
                                        input_arg=input_solved_expr
                                    }
                            CompositionRight ->
                                let 
                                    inner_expressions = map (extractProgram proof_graph) goal_node_predecessors
                                    term_name = CategoryData.name goal_category
                                in
                                    Just $ MTPLExpressionData.Construction{
                                        obj_name=term_name,
                                        composition_type=CategoryData.Composition,
                                        inner_expression=map fromJust inner_expressions
                                    }

{- program extraction -}       

-- Category Sequent rules

{-
[
            # no added sequent stuff
            ProductCategoryReductionLeft,
            MorphismReductionRight,
            # AND sequents
            ProductCategoryReductionRight,
            MorphismReductionLeft,
            CompositionReductionLeft,
            CompositionReductionRight,
            SumpositionReductionRight,
            # OR sequents
            SumCategoryReductionRight,
            SumCategoryReductionLeft,
            SumpositionReductionLeft,
            # level reduction
            HigherCategoryReductionLeft,
            HigherCategoryReductionRight,
            PlaceholderReductionRight
        ]
-}

categorySolverRules :: [SequentRule]
categorySolverRules = [
        -- no additional sequents
        morphismRight,
        productLeft,
        -- ANDs
        compositionRight,
        sumpositionRight,
        productRight,
        compositionLeft,
        morphismLeft,
        -- ORs
        sumRight,
        sumLeft,
        sumpositionLeft
        -- level reduction
    ]

{- product left -}
productLeftTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
productLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Product, CategoryData.inner=inner_categories} = 
    let 
        algra_edge_creator term = 
            Algra.edge 
                Edge{rule=[productLeft],inner_term_id=[CategoryData.name term]}
                Node{sequent_term=input_category}
                Node{sequent_term=term}
        all_edges = map algra_edge_creator inner_categories
    in
        (
            [foldr overlay (head all_edges) (tail all_edges)],
            Group {
                group_type=AND,
                sequents=[
                    Seq {
                        left_terms= filter (/= input_category) (left_terms sequent) ++ inner_categories,
                        right_term =right_term sequent 
                    }
                ]
            }
        )

productLeft :: SequentRule
productLeft = 
    Rule {
        rule_type=ProductLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Product,
        move_applier=productLeftTermApplier
    }
{- end product left -}

{- product right -}
productRightTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
productRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Product && comp_type /= CategoryData.Composition = error "bad argument"
    | otherwise =
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[productRight],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=term}
                    Node{sequent_term=input_category}
            all_edges = map algra_edge_creator inner_categories
        in
            (
                [foldr overlay (head all_edges) (tail all_edges)],
                Group {
                    group_type=AND,
                    sequents= map (\term -> Seq {
                            left_terms=left_terms sequent,
                            right_term=term 
                        }) inner_categories
                }
            )

productRight :: SequentRule
productRight = 
    Rule {
        rule_type=ProductRight,
        is_left=False, -- check right category
        term_checker=CategoryData.isCompositeType CategoryData.Product,
        move_applier=productRightTermApplier
    }
{- end product right -}

{- sum left -}
sumLeftTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
sumLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Sum && comp_type /= CategoryData.Sumposition = error "bad argument"
    | otherwise = 
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[sumLeft],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=input_category}
                    Node{sequent_term=term}
            all_edges = map algra_edge_creator inner_categories
            proof_edges = foldr overlay (head all_edges) (tail all_edges)
        in
            (
                [foldr overlay (head all_edges) (tail all_edges)],
                Group {
                    group_type=AND,
                    sequents= map (\term -> Seq {
                            left_terms= term:filter (/= input_category) (left_terms sequent),
                            right_term=right_term sequent 
                        }) inner_categories
                }
            )

sumLeft :: SequentRule
sumLeft = 
    Rule {
        rule_type=SumLeft,
        is_left=True, -- check right category
        term_checker=CategoryData.isCompositeType CategoryData.Sum,
        move_applier=sumLeftTermApplier
    }
{- end sum left -}

{- sum right -}
sumRightTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
sumRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Sum && comp_type /= CategoryData.Sumposition = error "bad argument"
    | otherwise =
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[sumRight],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=term}
                    Node{sequent_term=input_category}
            all_edges = map algra_edge_creator inner_categories
        in
            (
                all_edges,
                Group {
                    group_type=OR,
                    sequents= map (\term -> Seq {
                            left_terms= left_terms sequent,
                            right_term=term
                        }) inner_categories
                }
            )

sumRight :: SequentRule
sumRight = 
    Rule {
        rule_type=SumRight,
        is_left=False, -- check right category
        term_checker=CategoryData.isCompositeType CategoryData.Sum,
        move_applier=sumRightTermApplier
    }
{- end sum right -}

{- morphism left -}
morphismLeftTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
morphismLeftTermApplier sequent input_category@CategoryData.Morphism{CategoryData.input=morphism_input, CategoryData.output=morphism_output} = 
    let 
        input_edge = 
            Algra.edge 
                Edge{rule=[morphismLeft],inner_term_id=["input"]}
                Node{sequent_term=morphism_input}
                Node{sequent_term=morphism_output}
        morphism_edge = 
            Algra.edge 
                Edge{rule=[morphismLeft],inner_term_id=["morphism"]}
                Node{sequent_term=input_category}
                Node{sequent_term=morphism_output}
        proof_terms = Algra.overlay input_edge morphism_edge
        filtered_left_terms = filter (/= input_category) (left_terms sequent)
    in
        (
            [proof_terms],
            Group {
                group_type=AND,
                sequents=[
                    Seq {
                        left_terms=filtered_left_terms,
                        right_term=morphism_input
                    },
                    Seq {
                        left_terms=morphism_output:filtered_left_terms,
                        right_term=right_term sequent
                    }
                ]
            }
        )

morphismLeft :: SequentRule
morphismLeft = 
    Rule {
        rule_type=MorphismLeft,
        is_left=True, -- check right category
        term_checker=CategoryData.isMorphism,
        move_applier=morphismLeftTermApplier
    }
{- end morphism left -}

{- morphism right -}
morphismRightTermApplier:: Sequent -> SequentTerm -> ([Proof], SequentGroup)
morphismRightTermApplier sequent input_category@CategoryData.Morphism{CategoryData.input=morphism_input, CategoryData.output=morphism_output} = 
    let 
        input_edge = 
            Algra.edge 
                Edge{rule=[morphismRight],inner_term_id=["assumed"]}
                Node{sequent_term=morphism_input}
                Node{sequent_term=input_category}
        output_edge = 
            Algra.edge 
                Edge{rule=[morphismRight],inner_term_id=["to_prove"]}
                Node{sequent_term=morphism_output}
                Node{sequent_term=input_category}
        proof_terms = Algra.overlay input_edge output_edge
    in
        (
            [proof_terms],
            Group {
                group_type=AND,
                sequents=[
                    Seq {
                        left_terms=morphism_input:left_terms sequent,
                        right_term=morphism_output
                    }
                ]
            }
        )

morphismRight :: SequentRule
morphismRight = 
    Rule {
        rule_type=MorphismRight,
        is_left=False,
        term_checker=CategoryData.isMorphism,
        move_applier=morphismRightTermApplier
    }
{- end morphism right -}

{- sumpositionLeft -}
sumpositionLeftTermApplier :: Sequent -> SequentTerm -> ([Proof], SequentGroup)
sumpositionLeftTermApplier sequent input_category =
    let
        sum_result = sumLeftTermApplier sequent input_category
        proof = head $ fst sum_result
        edgeTransformer e@Edge{rule=[sumLeft]} = e{rule=[sumpositionLeft]}
        edgeTransformer e = e
    in
        (
            [Algra.emap edgeTransformer proof],
            snd sum_result
        ) 

sumpositionLeft :: SequentRule
sumpositionLeft = 
    Rule {
        rule_type=SumpositionLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Sumposition,
        move_applier=sumpositionLeftTermApplier
    }
{- end sumpositionLeft -}


{- sumpositionLeft -}
sumpositionRightTermApplier :: Sequent -> SequentTerm -> ([Proof], SequentGroup)
sumpositionRightTermApplier sequent input_category =
    let
        sum_result = sumRightTermApplier sequent input_category
        proofs = fst sum_result
        edgeTransformer e@Edge{rule=[sumRight]} = e{rule=[sumpositionRight]}
        edgeTransformer e = e
    in
        (
            [foldr (overlay . Algra.emap edgeTransformer) Algra.empty proofs],
            (snd sum_result){group_type=AND}
        ) 

sumpositionRight :: SequentRule
sumpositionRight = 
    Rule {
        rule_type=SumpositionRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Sumposition,
        move_applier=sumpositionRightTermApplier
    }
{- end sumpositionLeft -}

{- compositionLeft -}
compositionLeftTermApplier :: Sequent -> SequentTerm -> ([Proof], SequentGroup)
compositionLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Composition, CategoryData.inner=inner} =
    let
        morphized_input_category = CategoryCore.asMorphism input_category
        morph_result = morphismLeftTermApplier sequent morphized_input_category
        proofs = fst morph_result
        edgeTransformer e@Edge{rule=[morphismLeft]} = e{rule=[compositionLeft]}
        edgeTransformer e = e
        result_sequents = sequents $ snd morph_result
        filter_left_terms to_filter_sequent = Seq {
            left_terms=filter (/= input_category) (left_terms to_filter_sequent),
            right_term=right_term to_filter_sequent
        }
        nodeTransformer n@Node{sequent_term=term}
            | morphized_input_category == term = vertex Node{sequent_term=input_category}
            | otherwise = vertex n
        transformed_edges = Algra.emap edgeTransformer (head $ fst morph_result)
        transformed_proof = Algra.foldg Algra.empty nodeTransformer connect transformed_edges
    in
        (
            [transformed_proof],
            Group {
                group_type=AND,
                sequents=map filter_left_terms result_sequents
            }
        ) 

compositionLeft :: SequentRule
compositionLeft = 
    Rule {
        rule_type=CompositionLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Composition,
        move_applier=compositionLeftTermApplier
    }
{- end compositionLeft -}

{- compositionRight -}
compositionRightTermApplier :: Sequent -> SequentTerm -> ([Proof], SequentGroup)
compositionRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Composition, CategoryData.inner=inner} =
    let
        prod_result = productRightTermApplier sequent input_category
        edgeTransformer e@Edge{rule=[productRight]} = e{rule=[compositionRight]}
        edgeTransformer e = e
    in
        (
            [Algra.emap edgeTransformer (head $ fst prod_result)],
            snd prod_result
        ) 

compositionRight :: SequentRule
compositionRight = 
    Rule {
        rule_type=CompositionRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Composition,
        move_applier=compositionRightTermApplier
    }
{- end compositionLeft -}