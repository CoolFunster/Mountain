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

        let a_b = Composite Product [a,b]

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

        let a_b = Composite Product [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a,b],
            right_term=a_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [productRight] seq_to_solve)
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Composite Product [a, b])
    it "(Sum Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite Sum [a,b]

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

        let a_b = Composite Sum [a,b]

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

        let a_b = Composite Sum [a,b]
        -- let b_a = Composite (Name "b^a") Product [b,a]

        let a_or_c = Composite Sum [a,c]
        let b_or_c = Composite Sum [b,c]
        let a_or_c_and_b_or_c = Composite Product [a_or_c, b_or_c]

        let seq_to_solve = categorySequent {
            left_terms=[a_b, c],
            right_term=a_or_c_and_b_or_c
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Composite Product [c,c])
    it "(Morphism Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")

        let a_to_b = Morphism a b
        
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
        
        let a_b = Composite Product [a,b]

        let ab_to_b = Morphism a_b b

        let seq_to_solve = categorySequent {
            left_terms=[],
            right_term=ab_to_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Morphism a_b (Dereference a_b (Name "b")))
    it "(Sumposition Left) term applier" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_to_b = Morphism a b
        let b_to_a = Morphism b a

        let all = Composite Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[all],
            right_term=a
        }
        -- print (sumpositionLeftTermApplier seq_to_solve all)
        show (sumpositionLeftTermApplier seq_to_solve all) `shouldNotBe` "" 
    it "(Sumposition Right) term applier" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_to_b = Morphism a b
        let b_to_a = Morphism b a

        let all = Composite Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[a],
            right_term=all
        }
        -- print (sumpositionRightTermApplier seq_to_solve all)
        show (sumpositionRightTermApplier seq_to_solve all) `shouldNotBe` ""
    it "(Composition Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")

        let a_to_b = Morphism a b
        let b_to_c = Morphism b c
        let c_to_d = Morphism c d
        let a_to_d = Morphism a d

        let comp = Composite Composition [a_to_b, b_to_c]

        let seq_to_solve = categorySequent {
            left_terms=[comp, c_to_d],
            right_term=a_to_d
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        show program_from_proof `shouldNotBe` ""
    it "(Composition Right)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")
        let c = Thing (Name "c")
        let d = Thing (Name "d")

        let a_to_b = Morphism a b
        let b_to_c = Morphism b c

        let comp = Composite Composition [a_to_b, b_to_c]

        let seq_to_solve = categorySequent {
            left_terms=[a_to_b, b_to_c],
            right_term=comp
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        show program_from_proof `shouldNotBe` ""
    it "(Higher Left)" $ do
        let a = Thing (Name "a")
        let b = Thing (Name "b")

        let a_b = Composite Higher [a,b]

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

        let a_to_b = Morphism a b
        let b_to_c = Morphism b c
        let c_to_d = Morphism c d

        let a_to_c = Morphism a c
        let b_to_d = Morphism b d

        let all = Composite Higher [a_to_b, b_to_c, c_to_d]
        -- let all = Composite Higher [a_to_c, b_to_d]

        let subset = Composite Higher [a_to_c, b_to_d]

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