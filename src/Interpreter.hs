module Interpreter where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Function
import           Data.Functor.Identity
import           Data.Generics.Labels
import           Data.Generics.Product
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as T
import           Debug.Trace
import           GHC.Generics
import           Lens.Micro.Platform

import           Error
import           Syntax.Abstract

type MonadInterpret m = (MonadReader Env m, MonadError InterpretationError m)

newtype Interpreter a = Interpret
  { unInterpret :: ReaderT Env (Except InterpretationError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError InterpretationError
             , MonadReader Env
             )

runInterpreter :: Env -> Interpreter a -> Either Error a
runInterpreter env =
  runExcept . withExcept Interpretation . flip runReaderT env . unInterpret

liftMaybe = maybe (throwError Unimplemented) pure

interpretAtom :: (MonadInterpret m) => Atom -> m Atom
interpretAtom = pure

interpretValue :: (MonadInterpret m) => ValueExpr -> m Atom
interpretValue (VLambda arg body) = pure $ AClosure body arg
interpretValue (VIf test t f) = do
  ABoolean bool <- interpretValue test
  interpretValue $
    if bool
      then t
      else f
interpretValue (VSequence exprs) = last <$> traverse interpretValue exprs
interpretValue (VLet name body expr) = do
  res <- interpretValue body
  local (M.insert name res) $ interpretValue expr
interpretValue (VApplication app argVal) = do
  argVal' <- interpretValue argVal
  AClosure body argName <- interpretValue app
  interpretValue $ replaceSymbol argName argVal' body -- FIXME: this is ugly
  -- local (M.insert argName argVal') $ interpretValue body -- FIXME: this should work
interpretValue (VVector exprs) = undefined -- AVector <$> traverse interpretValue exprs
interpretValue (VSymbol sym) = liftMaybe . M.lookup sym =<< ask
interpretValue (VAtom atom) = interpretAtom atom

replaceWith :: (ValueExpr -> Maybe ValueExpr) -> ValueExpr -> ValueExpr
replaceWith fn = descend (\vex -> fromMaybe vex $ fn vex)

replace subst = replaceWith (`M.lookup` subst)

replaceSymbol :: Name -> Atom -> ValueExpr -> ValueExpr
replaceSymbol name atom = replace [(VSymbol name, VAtom atom)]
