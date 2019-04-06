module Type.Monad
  ( freshExistential
  , freshUniversal
  , typeerror
  , MonadInfer
  , Infer
  )
where

import           Application                    ( Compile )
import           Error                          ( Error
                                                , TypeError
                                                )
import qualified Error
import           Type.Types


type MonadInfer s q m
  = ( MonadError Error m
    , MonadState s m, HasType VariableId s
    )
type Infer a = forall s q m. (MonadInfer s q m) => m a

currentVariableId = typed @VariableId

freshName :: Infer VariableId
freshName = do
  varid <- use currentVariableId
  modifying currentVariableId succ
  pure varid

freshExistential :: Infer (Variable 'Existential)
freshExistential = Variable <$> freshName

freshUniversal :: Infer (Variable 'Universal)
freshUniversal = Variable <$> freshName

typeerror :: TypeError -> Infer a
typeerror err = throwError $ Error.Type err
