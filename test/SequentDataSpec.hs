module SequentDataSpec (spec) where

import Test.Hspec
import SequentData
import CategoryData
import MTPLExpressionData
import qualified Data.Set as Set
import Data.List (find)

import qualified Algebra.Graph.Labelled as Algra
import qualified Algebra.Graph.AdjacencyMap as Alam
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AlamAlg
import qualified Algebra.Graph.Export.Dot as AlgraDot
import qualified Algebra.Graph.ToGraph as ToGraph

spec :: Spec
spec = do
  describe "solveCategorySequent" $ do
    it "Returns nothing if unsolveable" $ do
        let a = Thing "a"

        let seq_to_solve = Seq {
            left_terms=[],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve        
        final_proof `shouldBe` noProof
        extractProgram final_proof Node{sequent_term=a} `shouldBe` Nothing
    it "(Product Left)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Product [a,b]

        let seq_to_solve = Seq {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=a}
        program_from_proof `shouldBe` Just (Dereference (Object a_b) "a")
    it "(Product Right)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Product [a,b]

        let seq_to_solve = Seq {
            left_terms=[a,b],
            right_term=a_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [productRight] seq_to_solve)
        let program_from_proof = extractProgram final_proof Node{sequent_term=a_b}
        program_from_proof `shouldBe` Just (Construction "a_b" Product [Object a, Object b])
    it "(Sum Right)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Sum [a,b]

        let seq_to_solve = Seq {
            left_terms=[a],
            right_term=a_b
        }
        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [sumLeft] seq_to_solve)
        let program_from_proof = extractProgram final_proof Node{sequent_term=a_b}
        program_from_proof `shouldBe` Just (Object a)
    it "(Sum Left)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "aVb" Sum [a,b]

        let seq_to_solve = Seq {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        final_proof `shouldBe` noProof
    it "(Compound Sum Left and Right)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_b = Composite "aVb" Sum [a,b]
        -- let b_a = Composite "b^a" Product [b,a]

        let a_or_c = Composite "aVc" Sum [a,c]
        let b_or_c = Composite "bVc" Sum [b,c]
        let a_or_c_and_b_or_c = Composite "(aVc)_(bVc)" Product [a_or_c, b_or_c]

        let seq_to_solve = Seq {
            left_terms=[a_b, c],
            right_term=a_or_c_and_b_or_c
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=a_or_c_and_b_or_c}
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Construction "(aVc)_(bVc)" Product [Object c,Object c])
    it "(Morphism Left)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_to_b = Morphism "a->b" a b
        
        let seq_to_solve = Seq {
            left_terms=[a_to_b, a],
            right_term=b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=b}
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Call (Object a_to_b) (Object a))
    it "(Morphism Right)" $ do
        let a = Thing "a"
        let b = Thing "b"
        
        let a_b = Composite "ab" Product [a,b]

        let ab_to_b = Morphism "ab_to_b" a_b b

        let seq_to_solve = Seq {
            left_terms=[],
            right_term=ab_to_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=ab_to_b}
        -- print program_from_proof
        program_from_proof `shouldBe` Just (MorphismChain "ab_to_b" [Object a_b, Dereference (Object a_b) "b"])
    it "(Sumposition Left) term applier" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_to_b = Morphism "a->b" a b
        let b_to_a = Morphism "b->a" b a

        let all = Composite "all" Sumposition [a_to_b, b_to_a]

        let seq_to_solve = Seq {
            left_terms=[all],
            right_term=a
        }
        -- print $ (prettyPrintProofStr . head . fst) (sumpositionLeftTermApplier seq_to_solve all)
        
        (prettyPrintProofStr . head . fst) (sumpositionLeftTermApplier seq_to_solve all) `shouldBe` "digraph Proof\n{\n  \"Node{Morphism{a->b,Thing{a}->Thing{b}}}\"\n  \"Node{Morphism{b->a,Thing{b}->Thing{a}}}\"\n  \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\"\n  \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\" -> \"Node{Morphism{a->b,Thing{a}->Thing{b}}}\" [label=\"[SequentRule{SumpositionLeft}],[a->b]\"]\n  \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\" -> \"Node{Morphism{b->a,Thing{b}->Thing{a}}}\" [label=\"[SequentRule{SumpositionLeft}],[b->a]\"]\n}\n"

    it "(Sumposition Right) term applier" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_to_b = Morphism "a->b" a b
        let b_to_a = Morphism "b->a" b a

        let all = Composite "all" Sumposition [a_to_b, b_to_a]

        let seq_to_solve = Seq {
            left_terms=[a],
            right_term=all
        }

        (prettyPrintProofStr . head . fst) (sumpositionRightTermApplier seq_to_solve all) `shouldBe` "digraph Proof\n{\n  \"Node{Morphism{a->b,Thing{a}->Thing{b}}}\"\n  \"Node{Morphism{b->a,Thing{b}->Thing{a}}}\"\n  \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\"\n  \"Node{Morphism{a->b,Thing{a}->Thing{b}}}\" -> \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\" [label=\"[SequentRule{SumpositionRight}],[a->b]\"]\n  \"Node{Morphism{b->a,Thing{b}->Thing{a}}}\" -> \"Node{all:|Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->a,Thing{b}->Thing{a}}|}\" [label=\"[SequentRule{SumpositionRight}],[b->a]\"]\n}\n"
    it "(Composition Left)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"
        let d = Thing "d"

        let a_to_b = Morphism "a->b" a b
        let b_to_c = Morphism "b->c" b c
        let c_to_d = Morphism "c->d" c d
        let a_to_d = Morphism "a->d" a d

        let comp = Composite "a_to_c" Composition [a_to_b, b_to_c]

        let seq_to_solve = Seq {
            left_terms=[comp, c_to_d],
            right_term=a_to_d
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=a_to_d}
        -- print program_from_proof
        show program_from_proof `shouldBe` "Just Chain{\"a->d\",[Object{Thing{a}},Call{Object{Morphism{c->d,Thing{c}->Thing{d}}}[Call{Object{a_to_c:(Morphism{a->b,Thing{a}->Thing{b}},Morphism{b->c,Thing{b}->Thing{c}})}[Object{Thing{a}}]}]}]}"
    it "(Composition Right)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"
        let d = Thing "d"

        let a_to_b = Morphism "a->b" a b
        let b_to_c = Morphism "b->c" b c

        let comp = Composite "a_to_c" Composition [a_to_b, b_to_c]

        let seq_to_solve = Seq {
            left_terms=[a_to_b, b_to_c],
            right_term=comp
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgram final_proof Node{sequent_term=comp}
        show program_from_proof `shouldBe` "Just Construction{\"a_to_c\",Composition,[Object{Morphism{a->b,Thing{a}->Thing{b}}},Object{Morphism{b->c,Thing{b}->Thing{c}}}]}"