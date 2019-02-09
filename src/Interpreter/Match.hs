module Interpreter.Match where

import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map

import           Classes
import           Interpreter.Types
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term


match :: Pattern -> Term -> Maybe (Map Reference.Value Term)
match (Pattern.Wildcard _   ) term = Just mempty
match (Pattern.Symbol _ name) term = Just $ Map.singleton name term
match (Pattern.Vector _ patterns) (Term.Vector _ values) | length patterns == length values =
  concatMapM (uncurry match) $ zip patterns values
match (Pattern.Tuple _ patternMap) (Term.Tuple _ valueMap) = do
  let missingValue   = Map.traverseMissing $ \_ _ -> Nothing
      missingPattern = Map.dropMissing
      matched        = Map.zipWithAMatched $ \_ pattrn value -> match pattrn value
  resultMap <- Map.mergeA missingValue missingPattern matched patternMap valueMap
  pure $ fold resultMap
match (Pattern.Record _ patternMap) (Term.Record _ valueMap) = do
  let missingValue   = Map.traverseMissing $ \_ _ -> Nothing
      missingPattern = Map.dropMissing
      matched        = Map.zipWithAMatched $ \_ pattrn value -> match pattrn value
  resultMap <- Map.mergeA missingValue missingPattern matched patternMap valueMap
  pure $ fold resultMap
match (Pattern.Variant _ patternTag patternBody) (Term.Variant _ termTag termBody)
  | patternTag == termTag = match patternBody termBody
match (Pattern.Atom _ patternAtom) (Term.Atom _ valueAtom) | patternAtom == valueAtom = Just mempty
match _ _ = Nothing
