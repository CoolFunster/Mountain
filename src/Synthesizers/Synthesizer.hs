module Synthesizers.Synthesizer where

import Data.List (find)
import Data.Map

data SynthesisResult a = NotPossible | Answer a | Unknown [a]

isNotPossible :: SynthesisResult a -> Bool
isNotPossible NotPossible = True
isNotPossible _ = False

isAnswered :: SynthesisResult a -> Bool
isAnswered (Answer _) = True
isAnswered _ = False

isUnknown :: SynthesisResult a -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

data Synthesizer a = Synthesizer {
    applicable :: a -> a -> Bool,
    synthesizer :: a -> a -> SynthesisResult a
}

synthesize :: [Synthesizer a] -> a -> a -> SynthesisResult a
synthesize [] _ _ = Unknown []
synthesize solver_list big_category small_category =
    case head_result of
        Unknown head_unknown ->
            case synthesize (tail solver_list) big_category small_category of
                Unknown tail_unknown -> Unknown (head_unknown ++ tail_unknown)
                NotPossible -> NotPossible
                Answer x -> Answer x
        NotPossible -> NotPossible
        Answer x -> Answer x
    where
        solve big_category small_category solver =
            if applicable solver big_category small_category
                then synthesizer solver big_category small_category
                else Unknown []
        head_result = solve big_category small_category (head solver_list)
