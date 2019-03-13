{-# LANGUAGE DataKinds #-}

module Type.Monad where

import           Error
import           Type.Types


newtype InferT m a =
  InferT { unInferT :: ExceptT TypeError (StateT State m) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState State,
            MonadError TypeError)

type Infer a = InferT Identity a

newtype NameSupply = NameSupply (NonEmpty Text) deriving (Generic)
instance Show NameSupply where
  show (NameSupply (name :| _)) = "NameSupply [" <> toString name <> ", ...]"

data State = State { nameSupply :: NameSupply } deriving (Show, Generic)

run :: Infer a -> (Either TypeError a, State)
run = unInferT >>> runExceptT >>> (`runStateT` initialState) >>> runIdentity
 where
  letters =
    greekAlphabet <> concatMap (\n -> map (\l -> l <> show n) greekAlphabet) ([1 ..] :: [Integer])
  initialState = State
    { nameSupply = NameSupply
      $ fromMaybe (error "[typechecking] empty variable name list")
      $ nonEmpty letters
    }

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
