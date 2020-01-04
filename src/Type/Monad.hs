module Type.Monad
  ( freshExistential
  , freshUniversal
  , freshMark
  , typeerror
  , lookup
  , MonadInfer
  , Infer
  , St(..)
  , nextVariableId
  )
where

import           Error                          ( Error
                                                , TypeError
                                                )
import qualified Error
import qualified Syntax.Reference as Ref
import           Type.Types
import qualified Type.Context as Ctx


type MonadInfer s m
  = ( MonadError Error m
    , MonadState s m, HasType St s
    )
type Infer a = forall s m. (MonadInfer s m) => m a

data St = St { _nextVariable :: VariableId } deriving (Generic)

state = typed @St
nextVariableId = state . typed @VariableId

freshName :: Infer VariableId
freshName = do
  varid <- use nextVariableId
  modifying nextVariableId succ
  pure varid

freshExistential :: Infer (Variable 'Existential)
freshExistential = Variable <$> freshName

freshUniversal :: Infer (Variable 'Universal)
freshUniversal = Variable <$> freshName

freshMark :: Infer (Variable 'Mark)
freshMark = Variable <$> freshName

typeerror :: TypeError -> Infer a
typeerror err = throwError $ Error.Type err

lookup :: Ref.Value -> Context -> Infer (Type, Principality)
lookup ref context =
  case (Ctx.lookup context ref) of
    Just (typ, p) -> pure (typ, p)
    Nothing -> error $ "[type/monad] unknown binding"
