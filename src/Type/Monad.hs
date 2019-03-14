module Type.Monad where

import           Error                          ( Error
                                                , TypeError
                                                )
import qualified Error
import           Type.Types


type MonadInfer s m = (MonadError Error m, MonadState s m, HasField "nextTypeVar" s s Int Int)
type Infer a = forall m s. (MonadInfer s m) => m a

nextTypeVar = field @"nextTypeVar"

freshName :: Infer Int
freshName = do
  i <- use nextTypeVar
  modifying nextTypeVar succ
  pure i

freshExistential :: Infer (Variable 'Existential)
freshExistential = Var <$> freshName

freshUniversal :: Infer (Variable 'Universal)
freshUniversal = Var <$> freshName

typeerror :: TypeError -> Infer a
typeerror err = throwError $ Error.Type err
