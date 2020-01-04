module Application
  ( MonadCompile
  , Compile
  , App(..)
  , run
  )
where

import           Error                          ( Error )
import qualified Type.Types                    as Type
import Type.Monad (MonadInfer)
import qualified Type.Monad as Type


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
    , MonadInfer s m
    )
type Compile a = forall s m. (MonadCompile s m) => m a

data St = St { typechecking :: Type.St
             } deriving (Generic)

run :: App a -> Either Error a
run = unApp >>> runExceptT >>> evaluatingStateT initialState >>> runIdentity
  where initialState = St {
          typechecking = Type.St {  _nextVariable = Type.VariableId 1}
          }
