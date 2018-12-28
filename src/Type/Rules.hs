module Type.Rules where

import           Type.Types



data Judgement
  = TypeSynthesis Context
                  Term
  | TypeChecking Context
                 Term
                 (Type, Principality)
  | Subtyping Context
              Polarity
              Type
              Type
  | TypeEquivalence Context
                    Type
                    Type
  | SpineTyping Context
                [Term]
                (Type, Principality)
  | PrincipalityRecoveringSpineTyping Context
                                      [Term]
                                      (Type, Principality)
  | Instantiation Context Type (Type, Kind)
  | Return Context
  deriving (Generic, Show)

data SpineTypingRule
  = EmptySpine
  | ForallSpine
  | ImpliesSpine
  | FunctionSpine
  | ExistentialSpine
  deriving (Generic, Show)

data TypeCheckingRule
  = Rec
  | AtomIntroductionExistential
  | AtomIntroduction
  | ForallIntroduction
  | ExistsIntroduction
  | ImpliesIntroduction
  | ImpliesIntroductionBottom
  | WithIntroduction
  | FunctionIntroduction
  | FunctionIntroductionExistential
  | SumInjectionIntroduction
  | SumInjectionIntroductionExistential
  | ProductIntroduction
  | ProductIntroductionExistential
  | Nil
  | Cons
  | Sub
  deriving (Generic, Show)
