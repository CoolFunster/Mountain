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
        let a = Thing "a"

        let seq_to_solve = categorySequent {
            left_terms=[],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve        
        final_proof `shouldBe` noProof
        extractProgramFromSeqent final_proof seq_to_solve `shouldBe` Nothing
    it "(Product Left)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Product [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Dereference a_b "a")
    it "(Product Right)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Product [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a,b],
            right_term=a_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [productRight] seq_to_solve)
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Composite "a_b" Product [a, b])
    it "(Sum Right)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Sum [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a],
            right_term=a_b
        }
        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr (solveCategorySequent [sumLeft] seq_to_solve)
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just a
    it "(Sum Left)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "aVb" Sum [a,b]

        let seq_to_solve = categorySequent {
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

        let seq_to_solve = categorySequent {
            left_terms=[a_b, c],
            right_term=a_or_c_and_b_or_c
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Composite "(aVc)_(bVc)" Product [c,c])
    it "(Morphism Left)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"

        let a_to_b = Morphism "a->b" a b
        
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
        let a = Thing "a"
        let b = Thing "b"
        
        let a_b = Composite "ab" Product [a,b]

        let ab_to_b = Morphism "ab_to_b" a_b b

        let seq_to_solve = categorySequent {
            left_terms=[],
            right_term=ab_to_b
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        -- print program_from_proof
        program_from_proof `shouldBe` Just (Morphism "ab_to_b" a_b (Dereference a_b "b"))
    it "(Sumposition Left) term applier" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_to_b = Morphism "a->b" a b
        let b_to_a = Morphism "b->a" b a

        let all = Composite "all" Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[all],
            right_term=a
        }
        -- print $ (prettyPrintProofStr . head . fst) (sumpositionLeftTermApplier seq_to_solve all)
        show (sumpositionLeftTermApplier seq_to_solve all) `shouldBe` "Group {group_type = AND, sequents = [Individual {sequent = Seq {left_frozen_terms = [all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|], left_terms = [Morphism{a->b,`a`->`b`}], right_term = `a`}, proof_edge = Connect [SequentRule{SumpositionLeft}],[a->b] (Vertex Node{all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|}) (Vertex Node{Morphism{a->b,`a`->`b`}})},Individual {sequent = Seq {left_frozen_terms = [all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|], left_terms = [Morphism{b->a,`b`->`a`}], right_term = `a`}, proof_edge = Connect [SequentRule{SumpositionLeft}],[b->a] (Vertex Node{all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|}) (Vertex Node{Morphism{b->a,`b`->`a`}})}]}"
    it "(Sumposition Right) term applier" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_to_b = Morphism "a->b" a b
        let b_to_a = Morphism "b->a" b a

        let all = Composite "all" Sumposition [a_to_b, b_to_a]

        let seq_to_solve = categorySequent {
            left_terms=[a],
            right_term=all
        }

        show (sumpositionRightTermApplier seq_to_solve all) `shouldBe` "Group {group_type = AND, sequents = [Individual {sequent = Seq {left_frozen_terms = [], left_terms = [`a`], right_term = Morphism{a->b,`a`->`b`}}, proof_edge = Connect [SequentRule{SumpositionRight}],[a->b] (Vertex Node{Morphism{a->b,`a`->`b`}}) (Vertex Node{all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|})},Individual {sequent = Seq {left_frozen_terms = [], left_terms = [`a`], right_term = Morphism{b->a,`b`->`a`}}, proof_edge = Connect [SequentRule{SumpositionRight}],[b->a] (Vertex Node{Morphism{b->a,`b`->`a`}}) (Vertex Node{all:|Morphism{a->b,`a`->`b`},Morphism{b->a,`b`->`a`}|})}]}"
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

        let seq_to_solve = categorySequent {
            left_terms=[comp, c_to_d],
            right_term=a_to_d
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        show program_from_proof `shouldBe` "Just Morphism{a->d,`a`->Morphism{c->d,`c`->`d`}(a_to_c:(Morphism{a->b,`a`->`b`},Morphism{b->c,`b`->`c`})(`a`))}"
    it "(Composition Right)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"
        let d = Thing "d"

        let a_to_b = Morphism "a->b" a b
        let b_to_c = Morphism "b->c" b c

        let comp = Composite "a_to_c" Composition [a_to_b, b_to_c]

        let seq_to_solve = categorySequent {
            left_terms=[a_to_b, b_to_c],
            right_term=comp
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        show program_from_proof `shouldBe` "Just a_to_c:(Morphism{a->b,`a`->`b`},Morphism{b->c,`b`->`c`})"
    it "(Higher Left)" $ do
        let a = Thing "a"
        let b = Thing "b"

        let a_b = Composite "a_b" Higher [a,b]

        let seq_to_solve = categorySequent {
            left_terms=[a_b],
            right_term=a
        }

        let final_proof = solveCategorySequent seq_to_solve
        -- putStrLn $ prettyPrintProofStr final_proof
        let program_from_proof = extractProgramFromSeqent final_proof seq_to_solve
        program_from_proof `shouldBe` Just (Dereference a_b "a")
    it "(Higher Right)" $ do
        let a = Thing "a"
        let b = Thing "b"
        let c = Thing "c"
        let d = Thing "d"

        let a_to_b = Morphism "a->b" a b
        let b_to_c = Morphism "b->c" b c
        let c_to_d = Morphism "c->d" c d

        let a_to_c = Morphism "a->c" a c
        let b_to_d = Morphism "b->d" b d

        let all = Composite "all" Higher [a_to_b, b_to_c, c_to_d]
        -- let all = Composite "all" Higher [a_to_c, b_to_d]

        let subset = Composite "subset" Higher [a_to_c, b_to_d]

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
    --     let a = Thing "a"

    --     let an_a = Composite{name="an_a", composition_type=HigherLeft, inner=[a,b,c,d]}

    --     let an_a = Placeholder{name="an_a", ph_level=0, ph_category=a}

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
    --     let a = Thing "a"
    --     let b = Thing "b"
    --     let c = Thing "c"
    --     let d = Thing "d"

    --     let letters = Composite{name="letters", composition_type=HigherLeft, inner=[a,b,c,d]}

    --     let a_letter = Placeholder{name="a_letter", ph_level=0, ph_category=letters}
    --     let a_b_c_d = Composite{name="a or b or c or d", composition_type=Sum, inner=[a,b,c,d]}

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