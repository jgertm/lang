module Classes where

import qualified Universum.Unsafe as Unsafe

type family Context phase

data Direction
  = Up
  | Down

class Tree (t :: * -> *) p where
  walkM :: (Monad f) => Direction -> (t p -> f (t p)) -> t p -> f (t p)
  metaM ::
       (Applicative f, Tree t p, Tree t q)
    => (Context p -> f (Context q))
    -> t p
    -> f (t q)

ascendM, descendM :: (Tree t p, Monad m) => (t p -> m (t p)) -> t p -> m (t p)
[ascendM, descendM] = map walkM [Up, Down]

ascend, descend :: (Tree t p) => (t p -> t p) -> t p -> t p
[ascend, descend] = map (\t f -> runIdentity . t (pure . f)) [ascendM, descendM]

metaM_ :: (Monad m, Tree t p) => (Context p -> m a) -> t p -> m (t p)
metaM_ f = metaM (\e -> f e $> e)

meta :: (Tree t p, Tree t q) => (Context p -> Context q) -> t p -> t q
meta f = runIdentity . metaM (pure . f)

context :: (Tree t p) => t p -> Context p
context tree =
  let action :: (Tree t p, MonadWriter (First (Context p)) m) => t p -> m (t p)
      action =
        metaM
          (\ctx -> do
             tell $ First (Just ctx)
             pure ctx)
   in Unsafe.fromJust . getFirst . evalWriter $ action tree
