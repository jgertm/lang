{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prelude
  ( module X
  , module Prelude
  , Category.Category(..)
  , Foldable(..)
  , Foldable.find
  , Foldable.and
  , Foldable.or
  , (Category.<<<)
  , (Category.>>>)
  )
where

import           Control.Category               ( (.) )
import qualified Control.Category              as Category
import           Control.Monad.Except          as X
                                                ( Except
                                                , MonadError(..)
                                                , liftEither
                                                , runExcept
                                                , withExcept
                                                )
import           Control.Monad.Writer          as X
                                                ( MonadWriter
                                                , Writer
                                                , WriterT
                                                , runWriter
                                                , runWriterT
                                                , tell
                                                )
import qualified Data.Foldable                 as Foldable
import           Data.Generics.Product
import           Data.Generics.Product.Fields  as X
import           Data.Generics.Product.Typed   as X
import qualified Data.Map.Strict               as Map
import           GHC.OverloadedLabels
import           Lens.Micro.Platform           as X
                                         hiding ( to )
import           Text.Pretty.Simple
import           Universum                     as X
                                         hiding ( Constraint
                                                , Container(..)
                                                , Down
                                                , Natural
                                                , Product
                                                , State
                                                , Sum
                                                , TVar
                                                , Type
                                                , ap
                                                , empty
                                                , id
                                                , join
                                                , pi
                                                , product
                                                , some
                                                , sum
                                                , (.)
                                                )

instance (HasField field s t a b, Functor f, sft ~ (s -> f t)) =>
         IsLabel field ((a -> f b) -> sft) where
  fromLabel = field @field

evalWriter :: Writer w a -> w
evalWriter = snd . runWriter

evalWriterT :: (Monad m) => WriterT w m a -> m w
evalWriterT = map snd . runWriterT

liftMaybe :: (MonadError err m) => err -> Maybe a -> m a
liftMaybe err = maybe (throwError err) pure

groupBy :: (Ord k) => (a -> k) -> [a] -> Map k [a]
groupBy f = Map.unionsWith mappend . map (\x -> Map.singleton (f x) [x])

identity :: (Category.Category cat) => cat a a
identity = Category.id

any, all :: (Foldable t) => (a -> Bool) -> t a -> Bool
any = Foldable.any

all = Foldable.all

identical :: (Eq a) => [a] -> Bool
identical (x : xs) = all (== x) xs
identical _        = True

errorToMaybe :: Except e a -> Maybe a
errorToMaybe = rightToMaybe . runExcept

infixr 0 |>>

infixr 0 <<|

(|>>) :: a -> (a -> b) -> b
x |>> f = f x

(<<|) :: (a -> b) -> a -> b
f <<| x = f x

matches :: Getting (First a) s a -> s -> Bool
matches prism value = isJust $ preview prism value

inspect = traceM . toText . pShow
spy x = let rep = toText $ pShow x in trace rep x

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for = flip traverse
