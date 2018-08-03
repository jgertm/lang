module Renaming
  ( rename
  , runRenaming
  , renameExpr
  ) where

import           Control.Monad.Except
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M

import           Error
import           Syntax

type MonadRename m = (MonadError RenamingError m, MonadReader Env m)

newtype Env =
  Env (Map Name Int)
  deriving (Show)

newtype Renamer a = Rename
  { unRename :: ReaderT Env (Except RenamingError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError RenamingError
             , MonadReader Env
             )

rename = runRenaming . renameExpr

runRenaming :: Renamer a -> Either Error a
runRenaming =
  runExcept . withExcept Renaming . usingReaderT (Env M.empty) . unRename

alias :: (MonadRename m) => Name -> m Name
alias name = do
  Env map <- ask
  pure $
    case M.lookup name map of
      Nothing -> name
      Just i  -> name <> show i

add :: (MonadRename m) => Name -> m a -> m a
add name =
  local $ \(Env map) ->
    Env $ M.insertWith (\old new -> old + new + 1) name 0 map

check :: (MonadRename m) => Name -> m ()
check name = do
  Env map <- ask
  whenNothing_ (M.lookup name map) $ throwError $ UnknownSymbol name

-- TODO: include original name in metadata
renameExpr :: (MonadRename m) => ExprA ann -> m (ExprA ann)
renameExpr expr@(ESymbol m name) = do
  check name
  ESymbol m <$> alias name
renameExpr (ELambda m name body) =
  add name $ do
    name' <- alias name
    body' <- renameExpr body
    pure $ ELambda m name' body'
renameExpr (ELet m name binding body) =
  add name $ do
    name' <- alias name
    binding' <- renameExpr binding
    body' <- renameExpr body
    pure $ ELet m name' binding' body'
renameExpr (EApplication m fn arg) = do
  fn' <- renameExpr fn
  arg' <- renameExpr arg
  pure $ EApplication m fn' arg'
renameExpr expr = pure expr
