module Synthesizers.SequentSynthesizer where

import Debug.Trace (trace)

import Data.List (zip, find, groupBy, intercalate)
import Data.Maybe (fromJust) 
import qualified Data.Set as Set
import Algebra.Graph.Labelled as Algra
import qualified Algebra.Graph.Export.Dot as AlgraDot
import qualified Algebra.Graph.ToGraph as ToGraph

import CategoryData
import Synthesizers.Synthesizer

sequentSynthesizer :: Synthesizer Category
sequentSynthesizer = Synthesizer {
    applicable= \x y -> True,
    synthesizer=synthesizeWithSequents
}

synthesizeWithSequents :: Category -> Category -> SynthesisResult Category
synthesizeWithSequents big_category small_category = 
    case extractProgram (solveCategorySequent $ categorySequent{left_terms=[big_category],right_term=small_category}) [Node{sequent_term=big_category}] (Node{sequent_term=small_category}) of
        Just solved_category -> Answer solved_category
        Nothing -> NotPossible

type SequentTerm = CategoryData.Category

data Sequent = Seq {
    left_frozen_terms::[SequentTerm],
    left_terms::[SequentTerm],
    right_term::SequentTerm
} deriving (Show, Eq)

categorySequent :: Sequent
categorySequent = Seq {left_frozen_terms=[],left_terms=[],right_term=CategoryData.empty}

sequentSolved::Sequent -> Bool
sequentSolved Seq{left_frozen_terms=left_frozen_terms,left_terms=left_terms,right_term=right_term} = any (== right_term) left_terms || any (== right_term) left_frozen_terms

data SequentGroupGoalType = AND | OR deriving (Show,Eq)
data SequentGroup = 
    Individual {
        sequent::Sequent,
        proof_edge::Proof
    } |
    Group {
        group_type::SequentGroupGoalType,
        sequents::[SequentGroup]
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
    CompositionRight |
    HigherLeft |
    HigherRight |
    PlaceholderLeft |
    PlaceholderRight
    deriving (Show, Eq)

data SequentRule = Rule {
    rule_type::SequentRuleTypes,
    is_left::Bool,
    term_checker::SequentTerm -> Bool,
    move_applier::Sequent -> SequentTerm -> SequentGroup
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

solveSequentGroup :: [SequentRule] -> SequentGroup -> Proof
solveSequentGroup ruleset (i@Individual{sequent=seq_to_solve,proof_edge=proof_edge}) 
    | sequentSolved seq_to_solve = 
        let 
            output = Algra.overlay proof_edge (Algra.vertex Node{sequent_term=right_term seq_to_solve})
        in 
            -- trace ("proof for " ++ show seq_to_solve ++ "\n" ++ prettyPrintProofStr output) output
            output
    | otherwise = 
        case expandSequent ruleset seq_to_solve of
            -- Nothing -> trace ("NoProof for " ++ show seq_to_solve) noProof
            Nothing -> noProof
            Just sequent_group -> 
                let
                    result = solveSequentGroup ruleset sequent_group
                    output_proof = Algra.overlay proof_edge result
                in
                    if result == noProof
                        then noProof
                        -- else trace ("IND: " ++ show seq_to_solve ++ "\n" ++ show i ++ "\n" ++ prettyPrintProofStr output_proof) output_proof
                        else output_proof
solveSequentGroup ruleset (Group{group_type=group_type, sequents=seqs_to_solve}) = 
    let 
        inner_proof_steps = map (solveSequentGroup ruleset) seqs_to_solve
    in
        case group_type of
            AND -> if (not . any Algra.isEmpty) inner_proof_steps
                        then 
                            let 
                                output = Algra.overlays inner_proof_steps
                            in
                                -- trace ("AND: " ++ prettyPrintProofStr output) output 
                                output 
                        else noProof
            OR -> if (not . all Algra.isEmpty) inner_proof_steps
                        then
                            let 
                                output = fromJust $ find (not . Algra.isEmpty) inner_proof_steps
                            in
                                -- trace ("OR: " ++ prettyPrintProofStr output) output 
                                output  
                        else noProof

expandSequent :: [SequentRule] -> Sequent -> Maybe SequentGroup
expandSequent ruleset input_sequent = do
        let applicable_terms = filter (not . null . snd) $ zip ruleset (sequence (map findApplicableTerms ruleset) input_sequent)
        let first_valid_rule = find (not . null . snd) applicable_terms
        case first_valid_rule of
            Nothing -> Nothing
            Just (rule, possible_terms) -> Just $ (move_applier rule) input_sequent (head possible_terms)
            -- Just (rule, possible_terms) -> trace ("Expanding: \n" ++ show input_sequent ++ "\n" ++ show first_valid_rule ++ "\n" ++ show ((move_applier rule) input_sequent (head possible_terms)) ++ "\n") Just $ (move_applier rule) input_sequent (head possible_terms)


pruneProof :: Proof -> ProofNode -> Proof
pruneProof result_proof goal_node = 
    let
        reachable_from_solution_node = ToGraph.reachable goal_node (Algra.transpose result_proof)
    in
        Algra.induce (`elem` reachable_from_solution_node) result_proof

solveCategorySequent :: Sequent -> Proof
solveCategorySequent input_seq = pruneProof (solveSequentGroup categorySolverRules Individual{sequent=input_seq,proof_edge=noProof}) Node{sequent_term=right_term input_seq}

extractProgram :: Proof -> [ProofNode] -> ProofNode -> Maybe CategoryData.Category
extractProgram proof_graph source_nodes goal_node@Node{sequent_term=goal_category}
    | not $ Algra.hasVertex goal_node proof_graph = Nothing
    | goal_node `elem` source_nodes = Just goal_category
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
                                    Just $ CategoryData.Dereference (fromJust (extractProgram proof_graph source_nodes productLeftPred)) productLeftLabel  
                            ProductRight ->
                                let 
                                    inner_expressions = map (extractProgram proof_graph source_nodes) goal_node_predecessors
                                    term_name = CategoryData.name goal_category
                                in
                                    Just $ CategoryData.Composite {
                                        CategoryData.name=term_name,
                                        CategoryData.composition_type=CategoryData.Product,
                                        CategoryData.inner=map fromJust inner_expressions
                                    }
                            SumLeft ->
                                let
                                    inner_expressions = map (extractProgram proof_graph source_nodes) goal_node_predecessors
                                    substitute_nothings predecessor term = 
                                        case term of
                                            Nothing -> sequent_term predecessor
                                            Just x -> x
                                    apply_list_foo_to_list_args= zipWith ($)
                                in
                                    Just $ CategoryData.Composite {
                                        CategoryData.name=CategoryData.name goal_category,
                                        CategoryData.composition_type=CategoryData.Sum,
                                        CategoryData.inner=apply_list_foo_to_list_args (map substitute_nothings goal_node_predecessors) inner_expressions
                                    }
                            SumRight ->
                                let
                                    inner_solved_expr = head (map (extractProgram proof_graph source_nodes) goal_node_predecessors)
                                in
                                    inner_solved_expr
                            MorphismLeft ->
                                let
                                    input_solved_expr = fromJust $ extractProgram proof_graph source_nodes $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "input") related_preds_and_edges)
                                    morphism_solved_expr = fromJust $ extractProgram proof_graph source_nodes $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "morphism") related_preds_and_edges)
                                in
                                    Just $ CategoryData.MorphismCall {
                                        CategoryData.base_morphism=morphism_solved_expr,
                                        CategoryData.argument=input_solved_expr
                                    }
                            MorphismRight ->
                                let
                                    assumed_node = fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "assumed") related_preds_and_edges)
                                    derived_node = fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "to_prove") related_preds_and_edges)
                                    derived_output = fromJust $ extractProgram proof_graph (assumed_node:source_nodes) derived_node
                                in
                                    Just $ CategoryData.Morphism {
                                        name=CategoryData.name goal_category,
                                        input=sequent_term assumed_node,
                                        output=derived_output
                                    }
                            SumpositionLeft ->
                                let
                                    inner_expressions = map (fromJust . extractProgram proof_graph source_nodes) goal_node_predecessors
                                in
                                    Just $ CategoryData.Composite{
                                        CategoryData.name=CategoryData.name goal_category,
                                        CategoryData.composition_type=CategoryData.Sumposition,
                                        CategoryData.inner=inner_expressions
                                    }
                            SumpositionRight ->
                                let
                                    inner_expressions = map (fromJust . extractProgram proof_graph source_nodes) goal_node_predecessors
                                in
                                    Just $ CategoryData.Composite{
                                        CategoryData.name=CategoryData.name goal_category,
                                        CategoryData.composition_type=CategoryData.Sumposition,
                                        CategoryData.inner=inner_expressions
                                    }
                            CompositionLeft ->
                                let
                                    input_solved_expr = fromJust $ extractProgram proof_graph source_nodes $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "input") related_preds_and_edges)
                                    morphism_solved_expr = fromJust $ extractProgram proof_graph source_nodes $ fst $ fromJust (find (\(pred,edge) -> (head . inner_term_id) edge == "morphism") related_preds_and_edges)
                                in
                                    Just $ CategoryData.MorphismCall {
                                        base_morphism=morphism_solved_expr,
                                        argument=input_solved_expr
                                    }
                            CompositionRight ->
                                let 
                                    inner_expressions = map (extractProgram proof_graph source_nodes) goal_node_predecessors
                                    term_name = CategoryData.name goal_category
                                in
                                    Just $ CategoryData.Composite{
                                        CategoryData.name=term_name,
                                        CategoryData.composition_type=CategoryData.Composition,
                                        CategoryData.inner=map fromJust inner_expressions
                                    }
                            HigherLeft ->
                                let
                                    productLeftPred = (fst . head) related_preds_and_edges
                                    productLeftLabel = (head . inner_term_id . snd . head) related_preds_and_edges
                                in
                                    Just $ CategoryData.Dereference (fromJust (extractProgram proof_graph source_nodes productLeftPred)) productLeftLabel 
                            HigherRight ->
                                let 
                                    inner_expressions = map (extractProgram proof_graph source_nodes) goal_node_predecessors
                                    term_name = CategoryData.name goal_category
                                in
                                    Just $ CategoryData.Composite {
                                        CategoryData.name=term_name,
                                        CategoryData.composition_type=CategoryData.Higher,
                                        CategoryData.inner=map fromJust inner_expressions
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
        -- level reduction
        higherLeft,
        higherRight,
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
    ]

-- utility
proofEdgeCreator :: SequentRule -> [Char] -> SequentTerm -> SequentTerm -> Proof
proofEdgeCreator rule edge_label input_term output_term = 
    Algra.edge 
        Edge{rule=[rule],inner_term_id=[edge_label]}
        Node{sequent_term=input_term}
        Node{sequent_term=output_term}

{- product left -}
productLeftTermApplier:: Sequent -> SequentTerm -> SequentGroup
productLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Product, CategoryData.inner=inner_categories} = 
    let 
        algra_edge_creator term = 
            Algra.edge 
                Edge{rule=[productLeft],inner_term_id=[CategoryData.name term]}
                Node{sequent_term=input_category}
                Node{sequent_term=term}
        all_edges = map algra_edge_creator inner_categories
    in
        Individual {
            sequent=Seq {
                left_frozen_terms = input_category : left_frozen_terms sequent, 
                left_terms = filter (/= input_category) (left_terms sequent) ++ inner_categories,
                right_term =right_term sequent 
            },
            proof_edge=foldr overlay (head all_edges) (tail all_edges)
        }

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
productRightTermApplier:: Sequent -> SequentTerm -> SequentGroup
productRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Product && comp_type /= CategoryData.Composition = error "bad argument"
    | otherwise =
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[productRight],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=term}
                    Node{sequent_term=input_category}
        in
            Group {
                group_type=AND,
                sequents= map (\term -> Individual {
                    sequent=Seq {
                        left_frozen_terms=left_frozen_terms sequent,
                        left_terms=left_terms sequent,
                        right_term=term 
                    },
                    proof_edge=algra_edge_creator term
                }) inner_categories
            }

productRight :: SequentRule
productRight = 
    Rule {
        rule_type=ProductRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Product,
        move_applier=productRightTermApplier
    }
{- end product right -}

{- sum left -}
sumLeftTermApplier:: Sequent -> SequentTerm -> SequentGroup
sumLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Sum && comp_type /= CategoryData.Sumposition = error "bad argument"
    | otherwise = 
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[sumLeft],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=input_category}
                    Node{sequent_term=term}
        in
            Group {
                group_type=AND,
                sequents= map (\term -> Individual{
                    sequent=Seq {
                        left_frozen_terms = input_category : left_frozen_terms sequent, 
                        left_terms= term:filter (/= input_category) (left_terms sequent),
                        right_term=right_term sequent 
                    },
                    proof_edge=algra_edge_creator term
                }) inner_categories
            }

sumLeft :: SequentRule
sumLeft = 
    Rule {
        rule_type=SumLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Sum,
        move_applier=sumLeftTermApplier
    }
{- end sum left -}

{- sum right -}
sumRightTermApplier:: Sequent -> SequentTerm -> SequentGroup
sumRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=comp_type, CategoryData.inner=inner_categories}
    | comp_type /= CategoryData.Sum && comp_type /= CategoryData.Sumposition = error "bad argument"
    | otherwise =
        let 
            algra_edge_creator term = 
                Algra.edge 
                    Edge{rule=[sumRight],inner_term_id=[CategoryData.name term]}
                    Node{sequent_term=term}
                    Node{sequent_term=input_category}
        in
            Group {
                group_type=OR,
                sequents= map (\term -> Individual{ 
                    sequent=Seq {
                        left_frozen_terms = left_frozen_terms sequent,
                        left_terms=left_terms sequent,
                        right_term=term
                    },
                    proof_edge=algra_edge_creator term
                }) inner_categories
            }

sumRight :: SequentRule
sumRight = 
    Rule {
        rule_type=SumRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Sum,
        move_applier=sumRightTermApplier
    }
{- end sum right -}

{- morphism left -}
morphismLeftTermApplier:: Sequent -> SequentTerm -> SequentGroup
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
        filtered_left_terms = filter (/= input_category) (left_terms sequent)
        new_left_frozen_terms = input_category : left_frozen_terms sequent
    in
        Group {
            group_type=OR,
            sequents=[
                Group {
                    group_type=AND,
                    sequents=[
                        Individual {
                            sequent=Seq {
                                left_frozen_terms = new_left_frozen_terms,
                                left_terms=filtered_left_terms,
                                right_term=morphism_input
                            },
                            proof_edge=input_edge
                        },
                        Individual {
                            sequent=Seq {
                                left_frozen_terms = new_left_frozen_terms,
                                left_terms=morphism_output:filtered_left_terms,
                                right_term=right_term sequent
                            },
                            proof_edge=morphism_edge
                        }
                    ]
                },
                Individual{
                    sequent=Seq{
                        left_frozen_terms = new_left_frozen_terms,
                        left_terms=filtered_left_terms,
                        right_term=right_term sequent
                    },
                    proof_edge=Algra.empty
                }
            ]
        }

morphismLeft :: SequentRule
morphismLeft = 
    Rule {
        rule_type=MorphismLeft,
        is_left=True,
        term_checker=CategoryData.isMorphism,
        move_applier=morphismLeftTermApplier
    }
{- end morphism left -}

{- morphism right -}
morphismRightTermApplier:: Sequent -> SequentTerm -> SequentGroup
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
    in
        Individual {
            sequent=Seq {
                left_frozen_terms =left_frozen_terms sequent,
                left_terms=morphism_input:left_terms sequent,
                right_term=morphism_output
            },
            proof_edge=Algra.overlay input_edge output_edge
        }

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
sumpositionLeftTermApplier :: Sequent -> SequentTerm -> SequentGroup
sumpositionLeftTermApplier sequent input_category =
    let
        sum_result = sumLeftTermApplier sequent input_category
        edgeTransformer e@Edge{rule=[sumLeft]} = e{rule=[sumpositionLeft]}
        edgeTransformer e = e
        sequentGroupTransformer i@Individual{proof_edge=pe} = i{proof_edge=Algra.emap edgeTransformer pe}
        sequentGroupTransformer g@Group{sequents=sequents} = g{sequents=map sequentGroupTransformer sequents}
    in
        sequentGroupTransformer sum_result

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
sumpositionRightTermApplier :: Sequent -> SequentTerm -> SequentGroup
sumpositionRightTermApplier sequent input_category =
    let
        sum_result = sumRightTermApplier sequent input_category
        edgeTransformer e@Edge{rule=[sumRight]} = e{rule=[sumpositionRight]}
        edgeTransformer e = e
        sequentGroupTransformer i@Individual{proof_edge=pe} = i{proof_edge=Algra.emap edgeTransformer pe}
        sequentGroupTransformer g@Group{sequents=sequents} = g{sequents=map sequentGroupTransformer sequents}
    in
        sequentGroupTransformer sum_result{group_type=AND}

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
compositionLeftTermApplier :: Sequent -> SequentTerm -> SequentGroup
compositionLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Composition, CategoryData.inner=inner} =
    let
        morphized_input_category = CategoryData.asMorphism input_category
        morphism_input = input morphized_input_category
        morphism_output = output morphized_input_category
        input_edge = 
            Algra.edge 
                Edge{rule=[compositionLeft],inner_term_id=["input"]}
                Node{sequent_term=morphism_input}
                Node{sequent_term=morphism_output}
        morphism_edge = 
            Algra.edge 
                Edge{rule=[compositionLeft],inner_term_id=["morphism"]}
                Node{sequent_term=input_category}
                Node{sequent_term=morphism_output}
        proof_terms = Algra.overlay input_edge morphism_edge
        filtered_left_terms = filter (/= input_category) (left_terms sequent)
        new_left_frozen_terms = input_category : left_frozen_terms sequent
    in
        Group {
            group_type=OR,
            sequents=[
                Group {
                    group_type=AND,
                    sequents=[
                        Individual {
                            sequent=Seq {
                                left_frozen_terms = new_left_frozen_terms,
                                left_terms=filtered_left_terms,
                                right_term=morphism_input
                            },
                            proof_edge=input_edge
                        },
                        Individual {
                            sequent=Seq {
                                left_frozen_terms = new_left_frozen_terms,
                                left_terms=morphism_output:filtered_left_terms,
                                right_term=right_term sequent
                            },
                            proof_edge=morphism_edge
                        }
                    ]
                },
                Individual {
                    sequent=Seq{
                        left_frozen_terms=new_left_frozen_terms,
                        left_terms=filtered_left_terms,
                        right_term=right_term sequent
                    },
                    proof_edge=Algra.empty
                }
            ]
        }

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
compositionRightTermApplier :: Sequent -> SequentTerm -> SequentGroup
compositionRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Composition, CategoryData.inner=inner} =
    let
        prod_result = productRightTermApplier sequent input_category
        edgeTransformer e@Edge{rule=[productRight]} = e{rule=[compositionRight]}
        edgeTransformer e = e
        sequentGroupTransformer i@Individual{proof_edge=pe} = i{proof_edge=Algra.emap edgeTransformer pe}
        sequentGroupTransformer g@Group{sequents=sequents} = g{sequents=map sequentGroupTransformer sequents}
    in
        sequentGroupTransformer prod_result

compositionRight :: SequentRule
compositionRight = 
    Rule {
        rule_type=CompositionRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Composition,
        move_applier=compositionRightTermApplier
    }
{- end compositionLeft -}

{- higher left -}
higherLeftTermApplier:: Sequent -> SequentTerm -> SequentGroup
higherLeftTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=CategoryData.Higher, CategoryData.inner=inner_categories} = 
    let 
        algra_edge_creator term = 
            Algra.edge 
                Edge{rule=[higherLeft],inner_term_id=[CategoryData.name term]}
                Node{sequent_term=input_category}
                Node{sequent_term=term}
    in
        Individual {
            sequent=Seq {
                left_frozen_terms = input_category : left_frozen_terms sequent,
                left_terms= filter (/= input_category) (left_terms sequent) ++ inner_categories,
                right_term =right_term sequent 
            },
            proof_edge=Algra.overlays (map algra_edge_creator inner_categories)
        }

higherLeft :: SequentRule
higherLeft = 
    Rule {
        rule_type=HigherLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Higher,
        move_applier=higherLeftTermApplier
    }
{- end higher left -}

{- higher right -}
higherRightTermApplier:: Sequent -> SequentTerm -> SequentGroup
higherRightTermApplier sequent input_category@CategoryData.Composite{CategoryData.composition_type=Higher, CategoryData.inner=inner_categories} =
    let 
        algra_edge_creator term = 
            Algra.edge 
                Edge{rule=[higherRight],inner_term_id=[CategoryData.name term]}
                Node{sequent_term=term}
                Node{sequent_term=input_category}
    in
        Group {
            group_type=AND,
            sequents= map (\term -> Individual {
                sequent=Seq {
                    left_frozen_terms = left_frozen_terms sequent,
                    left_terms= left_terms sequent,
                    right_term=term
                },
                proof_edge=algra_edge_creator term
            }) inner_categories
        }

higherRight :: SequentRule
higherRight = 
    Rule {
        rule_type=HigherRight,
        is_left=False,
        term_checker=CategoryData.isCompositeType CategoryData.Higher,
        move_applier=higherRightTermApplier
    }
{- end higher right -}

{- placeholder left -}
placeholderLeftTermApplier :: Sequent -> SequentTerm -> SequentGroup
placeholderLeftTermApplier sequent input_category@CategoryData.Placeholder{ph_level=ph_level,ph_category=ph_category}
    | ph_level == level ph_category = 
        -- just take the ph_category
        Individual {
            sequent=Seq {
                left_frozen_terms = input_category : left_frozen_terms sequent,
                left_terms=ph_category : filter (/= input_category) (left_terms sequent),
                right_term=right_term sequent 
            },
            proof_edge=proofEdgeCreator placeholderLeft (CategoryData.name input_category) input_category ph_category
        }
    | otherwise =
        case ph_category of
            Thing{} -> error "Should never get this if its a valid ph since level should be > 0"
            Composite{composition_type=c_type,inner=inner} -> 
                if c_type == Higher 
                    then error "not implemented yet"
                    {- sum type of some limited number of generations of the category -}
                    {- if you can't generate all of the category then the rule doesn't apply -}
                    else
                        let
                            create_placeholder some_input_category = Placeholder{
                                name="ph_" ++ CategoryData.name some_input_category, 
                                ph_level = ph_level,
                                ph_category = some_input_category
                            }
                            term_to_add = Composite{
                                name=CategoryData.name input_category, 
                                composition_type=c_type,
                                inner=map create_placeholder inner
                            }
                        in
                            Individual {
                                sequent=Seq {
                                    left_frozen_terms = input_category : left_frozen_terms sequent,
                                    left_terms=term_to_add : filter (/= input_category) (left_terms sequent),
                                    right_term=right_term sequent 
                                },
                                proof_edge=proofEdgeCreator placeholderLeft (CategoryData.name input_category) input_category ph_category
                            } 


placeholderLeft :: SequentRule
placeholderLeft = 
    Rule {
        rule_type=HigherLeft,
        is_left=True,
        term_checker=CategoryData.isCompositeType CategoryData.Higher,
        move_applier=higherLeftTermApplier
    } 
{- end placeholder left -}