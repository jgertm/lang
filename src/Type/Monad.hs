{-# LANGUAGE DataKinds #-}

module Type.Monad where

import           Control.Monad.RWS.Lazy
import qualified Text.Show

import           Error
import           Type.Rules
import           Type.Types


newtype InferT m a =
  InferT { unInferT :: ExceptT TypeError (RWST () [Judgement] State m) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState State,
            MonadWriter [Judgement],
            MonadError TypeError)

type Infer a = InferT Identity a

newtype NameSupply = NameSupply (NonEmpty Text) deriving (Generic)
instance Show NameSupply where
  show (NameSupply (name :| _)) = "NameSupply [" <> toString name <> ", ...]"

data State = State { nameSupply :: NameSupply } deriving (Show, Generic)

run :: Infer a -> (Either TypeError a, State, [Judgement])
run = unInferT >>> runExceptT >>> (\m -> runRWST m () initialState) >>> runIdentity
 where
  letters =
    greekAlphabet <> concatMap (\n -> map (\l -> l <> show n) greekAlphabet) ([1 ..] :: [Integer])
  initialState = State
    { nameSupply = NameSupply
      $ fromMaybe (error "[typechecking] empty variable name list")
      $ nonEmpty letters
    }

judgement :: Judgement -> Infer ()
judgement rule = tell [rule]

freshName :: Infer Text
freshName = do
  NameSupply (name :| names) <- use (typed @NameSupply)
  assign
    (typed @NameSupply)
    (NameSupply $ fromMaybe (error "[typechecking] exhausted variable name supply") $ nonEmpty names
    )
  pure name

freshExistential :: Infer (Variable 'Existential)
freshExistential = Var <$> freshName

freshUniversal :: Infer (Variable 'Universal)
freshUniversal = Var <$> freshName

greekAlphabet :: IsString s => [s]
greekAlphabet =
  [ "alpha"
  , "beta"
  , "gamma"
  , "delta"
  , "epsilon"
  , "zeta"
  , "eta"
  , "theta"
  , "iota"
  , "kappa"
  , "lambda"
  , "mu"
  , "nu"
  , "xi"
  , "omicron"
  , "pi"
  , "rho"
  , "sigma"
  , "tau"
  , "upsilon"
  , "phi"
  , "chi"
  , "psi"
  , "omega"
  ]
