module Application
  ( App(..)
  , run
  )
where

import           Error                          ( Error )
import qualified Syntax.Reference              as Ref


newtype App a = App { unApp :: ExceptT Error (StateT St Identity) a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadState St,
            MonadError Error)

data St = St { nextTypeVar         :: Int
             , currentNameVersions :: Map Ref.Value Int
             } deriving (Generic)

run :: App a -> Either Error a
run = unApp >>> runExceptT >>> evaluatingStateT initialState >>> runIdentity
  where initialState = St {nextTypeVar = 1, currentNameVersions = mempty}
