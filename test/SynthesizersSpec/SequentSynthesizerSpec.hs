module SynthesizersSpec.SequentSynthesizerSpec (spec) where

import Test.Hspec
import Synthesizers.SequentSynthesizer
import CategoryData
import qualified Data.Set as Set
import Data.List (find)

import qualified Algebra.Graph.Labelled as Algra
import qualified Algebra.Graph.AdjacencyMap as Alam
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AlamAlg
import qualified Algebra.Graph.Export.Dot as AlgraDot
import qualified Algebra.Graph.ToGraph as ToGraph

extractProgramFromSeqent :: Proof -> Sequent -> Maybe Category
extractProgramFromSeqent proof seq = extractProgram proof (map (\x -> Node{sequent_term=x}) (left_terms seq)) Node{sequent_term=(right_term seq)}

spec :: Spec
spec = do
  describe "solveCategorySequent" $ do
    it "Returns nothing if unsolveable" $ do
        let a = Thing (Name "a")

        let seq_to_solve = categorySequent {
            left_terms=[],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve        
        final_proof `shouldBe` noProof
        extractProgramFromSeqent final_proof seq_to_solve `shouldBe` Nothing
    it "(Product Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite (Name "a_b") Product [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Dereference a_b (Name "a"))
    it "(Product Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite (Name "a_b") Product [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a,b],
            right_term=a_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [productRight] seq_to_solve)
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Composite (Name "a_b") Product [a, b])
    it "(Sum Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite (Name "a_b") Sum [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a],
            right_term=a_b
        }
        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [sumLeft] seq_to_solve)
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just a
    it "(Sum Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite (Name "aVb") Sum [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        final_proof `shouldBe` noProof
    it "(Compound Sum Left and Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")

        let a_b = Composite (Name "aVb") Sum [a,b]
        -- let b_a = Composite (Name "b^a") Product [b,a]

        let a_or_c = Composite (Name "aVc") Sum [a,c]
        let b_or_c = Composite (Name "bVc") Sum [b,c]
        let a_or_c_and_b_or_c = Composite (Name "(aVc)_(bVc)") Product [a_or_c, b_or_c]

        let seq_to_solve = categorySequent {
            left_terms=[a_b, c],
            right_term=a_or_c_and_b_or_c
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Composite (Name "(aVc)_(bVc)") Product [c,c])
    it "(Morphism Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")

        let a_to_b = Morphism (Name "a->b") a b
        
        let seq_to_solve = categorySequent {
            left_terms=[a_to_b, a],
            right_term=b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (MorphismCall a_to_b a)
    it "(Morphism Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        
        let a_b = Composite (Name "ab") Product [a,b]

        let ab_to_b = Morphism (Name "ab_to_b") a_b b

        let seq_to_solve = categorySequent {
            left_terms=[],
            right_term=ab_to_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Morphism (Name "ab_to_b") a_b (Dereference a_b (Name "b")))
    it "(Sumposition Left) term applier" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_to_b = Morphism (Name "a->b") a b
        let b_to_a = Morphism (Name "b->a") b a

        let all = Composite (Name "all") Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[all],
            right_term=a
        }
        -- print (sumpositionLeftTermApplier seq_to_solve all)
        show (sumpositionLeftTermApplier seq_to_solve all) `shouldBe` "Group {group_type = AND, sequents = [Individual {sequent = Seq {left_frozen_terms = [Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|], left_terms = [Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`}], right_term = `Name \"a\"`}, proof_edge = Connect [SequentRule{SumpositionLeft}],[Name \"a->b\"] (Vertex Node{Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|}) (Vertex Node{Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`}})},Individual {sequent = Seq {left_frozen_terms = [Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|], left_terms = [Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}], right_term = `Name \"a\"`}, proof_edge = Connect [SequentRule{SumpositionLeft}],[Name \"b->a\"] (Vertex Node{Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|}) (Vertex Node{Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}})}]}"
    it "(Sumposition Right) term applier" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_to_b = Morphism (Name "a->b") a b
        let b_to_a = Morphism (Name "b->a") b a

        let all = Composite (Name "all") Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[a],
            right_term=all
        }
        -- print (sumpositionRightTermApplier seq_to_solve all)
        show (sumpositionRightTermApplier seq_to_solve all) `shouldBe` "Group {group_type = AND, sequents = [Individual {sequent = Seq {left_frozen_terms = [], left_terms = [`Name \"a\"`], right_term = Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`}}, proof_edge = Connect [SequentRule{SumpositionRight}],[Name \"a->b\"] (Vertex Node{Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`}}) (Vertex Node{Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|})},Individual {sequent = Seq {left_frozen_terms = [], left_terms = [`Name \"a\"`], right_term = Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}}, proof_edge = Connect [SequentRule{SumpositionRight}],[Name \"b->a\"] (Vertex Node{Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}}) (Vertex Node{Name \"all\":|Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->a\",`Name \"b\"`->`Name \"a\"`}|})}]}"
    it "(Composition Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")

        let a_to_b = Morphism (Name "a->b") a b
        let b_to_c = Morphism (Name "b->c") b c
        let c_to_d = Morphism (Name "c->d") c d
        let a_to_d = Morphism (Name "a->d") a d

        let comp = Composite (Name "a_to_c") Composition [a_to_b, b_to_c]

        let seq_to_solve = categorySequent {
            left_terms=[comp, c_to_d],
            right_term=a_to_d
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        show program_from_proof `shouldBe` "Just Morphism{Name \"a->d\",`Name \"a\"`->Morphism{Name \"c->d\",`Name \"c\"`->`Name \"d\"`}(Name \"a_to_c\":(Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->c\",`Name \"b\"`->`Name \"c\"`})(`Name \"a\"`))}"
    it "(Composition Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")

        let a_to_b = Morphism (Name "a->b") a b
        let b_to_c = Morphism (Name "b->c") b c

        let comp = Composite (Name "a_to_c") Composition [a_to_b, b_to_c]

        let seq_to_solve = categorySequent {
            left_terms=[a_to_b, b_to_c],
            right_term=comp
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        show program_from_proof `shouldBe` "Just Name \"a_to_c\":(Morphism{Name \"a->b\",`Name \"a\"`->`Name \"b\"`},Morphism{Name \"b->c\",`Name \"b\"`->`Name \"c\"`})"
    it "(Higher Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite (Name "a_b") Higher [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Dereference a_b (Name "a"))
    it "(Higher Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")

        let a_to_b = Morphism (Name "a->b") a b
        let b_to_c = Morphism (Name "b->c") b c
        let c_to_d = Morphism (Name "c->d") c d

        let a_to_c = Morphism (Name "a->c") a c
        let b_to_d = Morphism (Name "b->d") b d

        let all = Composite (Name "all") Higher [a_to_b, b_to_c, c_to_d]
        -- let all = Composite (Name "all") Higher [a_to_c, b_to_d]

        let subset = Composite (Name "subset") Higher [a_to_c, b_to_d]

        let seq_to_solve = categorySequent {
            left_terms=[all],
            right_term=subset
        }

        let final_proof = solveCategorySequent seq_to_solve
        final_proof `shouldNotBe` noProof
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldNotBe` Nothing
    -- it "(Placeholder left) same level" $ do
    --     let a = Thing (Name "a")

    --     let an_a = Composite{name=Name "an_a", composition_type=HigherLeft, inner=[a,b,c,d]}

    --     let an_a = Placeholder{name=Name "an_a", ph_level=0, ph_category=a}

    --     let seq_to_solve = categorySequent {
    --         left_terms=[an_a],
    --         right_term=a
    --     }

    --     let final_proof = solveCategorySequent seq_to_solve
    --     final_proof `shouldNotBe` noProof
    --     -- putStrLn $ prettyPrintProofStr final_proof
    --     let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
    --     -- print program_from_proof
    --     program_from_proof `shouldNotBe` Nothing
    -- it "(Placeholder left) higher" $ do
    --     let a = Thing (Name "a")
    --     let b = Thing (Name "b")
    --     let c = Thing (Name "c")
    --     let d = Thing (Name "d")

    --     let letters = Composite{name=Name "letters", composition_type=HigherLeft, inner=[a,b,c,d]}

    --     let a_letter = Placeholder{name=Name "a_letter", ph_level=0, ph_category=letters}
    --     let a_b_c_d = Composite{name=Name "a or b or c or d", composition_type=Sum, inner=[a,b,c,d]}

    --     let seq_to_solve = categorySequent {
    --         left_terms=[a_letter],
    --         right_term=a_b_c_d
    --     }

    --     let final_proof = solveCategorySequent seq_to_solve
    --     final_proof `shouldNotBe` noProof
    --     -- putStrLn $ prettyPrintProofStr final_proof
    --     let program_from_proof = extractProgram final_proof [Node{sequent_term=all}] Node{sequent_term=a_b_c_d}
    --     -- print program_from_proof
    --     program_from_proof `shouldNotBe` Nothing