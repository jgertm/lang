{-# LANGUAGE LambdaCase #-}

module Interpreter where

import           Control.Monad.Except
import qualified Data.Map.Strict      as M
import qualified Universum.Unsafe     as Unsafe

import           Error
import           Syntax

type MonadInterpret m
   = (MonadReader Env m, MonadError InterpretationError m, MonadFail m)

type Env = Map Name Atom

newtype Interpreter a = Interpret
  { unInterpret :: ReaderT Env (Except InterpretationError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError InterpretationError
             , MonadReader Env
             )

interpret = runInterpreter mempty . interpretValue

instance MonadFail Interpreter where
  fail _ = throwError Semantics

runInterpreter :: Env -> Interpreter a -> Either Error a
runInterpreter env =
  runExcept . withExcept Interpretation . usingReaderT env . unInterpret

liftMaybe = maybe (throwError Unimplemented) pure

interpretAtom :: (MonadInterpret m) => Atom -> m Atom
interpretAtom = pure

interpretValue :: (MonadInterpret m) => ExprA ann -> m Atom
interpretValue (ELambda _ arg body) = pure $ AClosure (void body) arg
interpretValue (EIf _ test t f) = do
  ABoolean bool <- interpretValue test
  interpretValue $
    if bool
      then t
      else f
interpretValue (ESequence _ exprs) =
  Unsafe.last <$> traverse interpretValue exprs
interpretValue (ELet _ name body expr) = do
  res <- interpretValue body
  local (M.insert name res) $ interpretValue expr
interpretValue (EApplication _ app argVal) = do
  argVal' <- interpretValue argVal
  AClosure body argName <- interpretValue app
  interpretValue $ replaceSymbol argName argVal' body -- FIXME: this is ugly
  -- local (M.insert argName argVal') $ interpretValue body -- FIXME: this should work
interpretValue (EVector _ exprs) = undefined -- AVector <$> traverse interpretValue exprs
interpretValue (ESymbol _ sym) = liftMaybe . M.lookup sym =<< ask
interpretValue (EAtom _ atom) = interpretAtom atom

replaceWith :: (ExprA ann -> Maybe (ExprA ann)) -> ExprA ann -> ExprA ann
replaceWith fn = descend (\vex -> fromMaybe vex $ fn vex)

replaceSymbol :: Name -> Atom -> ExprA ann -> ExprA ann
replaceSymbol name atom =
  replaceWith
    (\case
       ESymbol ann name' ->
         if name == name'
           then Just (EAtom ann atom)
           else Nothing
       _ -> Nothing)
