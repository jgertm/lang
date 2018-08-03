module Prelude
  ( module X
  , module Prelude
  ) where

import           Control.Monad.Writer as X (MonadWriter, runWriter, runWriterT,
                                            tell)
import           Universum            as X hiding (Constraint, TVar, Type)

evalWriter = snd . runWriter

evalWriterT = snd . runWriterT
