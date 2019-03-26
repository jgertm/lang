module Application
  ( MonadCompile
  , Compile
  , App(..)
  , run
  )
where

import           Error                          ( Error )
import qualified Syntax.Reference              as Ref
import qualified Type.Types                    as Type


newtype App a = App { unApp :: ExceptT Error (StateT St Identity) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState St
           , MonadError Error
           )

instance MonadFail App where
  fail = error . toText

type MonadCompile s m
  = ( MonadError Error m
    , MonadFail m
    , MonadState s m, HasType Type.VariableId s, HasType Type.Context s
    )
type Compile a = forall s m. (MonadCompile s m) => m a

data St = St { nextTypeVar :: Type.VariableId
             , baseContext :: Type.Context
             } deriving (Generic)

run :: App a -> Either Error a
run = unApp >>> runExceptT >>> evaluatingStateT initialState >>> runIdentity
  where initialState = St {nextTypeVar = Type.VariableId 1, baseContext = Type.Context mempty}
