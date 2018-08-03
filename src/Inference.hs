module Inference where

import           Control.Monad.Except

import           Data.Generics.Product.Typed
import qualified Universum.Unsafe            as Unsafe

import           Annotation
import           Error
import           Syntax
import           Types

type MonadInfer m = (MonadError InferenceError m, MonadState Unique m)

type Unique = Int

newtype Infer a = Infer
  { unInfer :: StateT Int (Except InferenceError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError InferenceError
             , MonadState Unique
             )

runInfer :: Infer a -> Either Error a
runInfer = runExcept . withExcept Inference . evaluatingStateT 0 . unInfer

fresh :: (MonadInfer m) => m Unique
fresh = do
  var <- get
  modify succ
  pure var

inferAtom :: Atom -> Type
inferAtom AUnit        = unit
inferAtom (AInteger _) = integer
inferAtom (AString _)  = string
inferAtom (ABoolean _) = boolean

-- inferAtom (AClosure body _) = inferValue body
inferValue :: (MonadInfer m) => Expr -> m Type
inferValue (EAtom meta atom) = pure $ inferAtom atom
inferValue (ELet meta _ _ body) = inferValue body
inferValue (EVector meta els) = do
  elTypes <- traverse inferValue els
  case ordNub elTypes of
    [typ] -> pure $ vector typ
    []    -> undefined
    a:b:_ -> throwError $ TypeMismatch a b

label :: (MonadState Unique m) => ExprA Meta -> m (ExprA Meta)
label =
  metaM
    (\anns -> do
       tvar <- new
       pure $ Type tvar : anns)
  where
    new = do
      c <- get
      modify succ
      pure . Variable $ "t" <> show c

type Constraint = (Type, Type)

annotations :: ExprA Meta -> [Meta]
annotations = view (typed @[Meta])

typeAnnotation :: (Element t ~ Meta, Container t) => t -> Meta
typeAnnotation = Unsafe.fromJust . find match
  where
    match :: Meta -> Bool
    match (Type _) = True
    match _        = False

constrain :: (MonadWriter [Constraint] m) => ExprA Meta -> m (ExprA Meta)
constrain e@(EApplication m fn arg) = do
  let Type appTV = typeAnnotation m
      Type fnTV = typeAnnotation $ annotations fn
      Type argTV = typeAnnotation $ annotations arg
      fnT = Function argTV appTV
  tell [(fnTV, fnT)]
  pure e
constrain e@(ELambda m _ body) = do
  let Type lambdaTV = typeAnnotation m
      -- Type argTV = typeAnnotation $ annotations arg
      Type bodyTV = typeAnnotation $ annotations body
  tell [(lambdaTV, bodyTV)]
  pure e
constrain e@(ESequence m body) = do
  let result = Unsafe.last body
  let Type sequenceTV = typeAnnotation m
      Type resultTV = typeAnnotation $ annotations result
  tell [(sequenceTV, resultTV)]
  pure e
constrain e@(EAtom m atom) = do
  let Type atomTV = typeAnnotation m
      atomT = inferAtom atom
  tell [(atomTV, atomT)]
  pure e
constrain e@(ELet m _ _ body) = do
  let Type letTV = typeAnnotation m
      Type bodyTV = typeAnnotation $ annotations body
  tell [(letTV, bodyTV)]
  pure e
constrain e = pure e
-- TODO: need a dictionary for symbols
