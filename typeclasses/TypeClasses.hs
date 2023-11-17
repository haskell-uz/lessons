{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Wextra -Wcompat -Weverything -Wno-unsafe -Wno-missing-safe-haskell-mode #-}

module TypeClasses
  ( Eq
  , Ordering
  , Ord
  , Show
  , Semigroup
  , Monoid
  , Functor
  , Applicative
  , Monad
  , Foldable
  , Traversable
  )
where

import Data.Kind (Constraint, Type)
import Prelude (Bool, String)

type Eq :: Type -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (!=) :: a -> a -> Bool

type Ordering :: Type
data Ordering = LessThan | Equal | GreaterThan

type Ord :: Type -> Constraint
class (Eq a) => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool

type Show :: Type -> Constraint
class Show a where
  show :: a -> String

type Semigroup :: Type -> Constraint
class Semigroup a where
  combine :: a -> a -> a

type Monoid :: Type -> Constraint
class (Semigroup a) => Monoid a where
  neutral :: a

type Functor :: (Type -> Type) -> Constraint
class Functor f where
  map :: (a -> b) -> f a -> f b

type Applicative :: (Type -> Type) -> Constraint
class (Functor f) => Applicative f where
  pure :: a -> f a
  apply :: f (a -> b) -> f a -> f b

type Monad :: (Type -> Type) -> Constraint
class (Applicative f) => Monad f where
  flatMap :: (a -> f b) -> f a -> f b

type Foldable :: (Type -> Type) -> Constraint
class Foldable t where
  foldRight :: (a -> b -> b) -> b -> t a -> b

type Traversable :: (Type -> Type) -> Constraint
class (Functor t, Foldable t) => Traversable t where
  traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  sequence :: (Applicative f) => t (f a) -> f (t a)
