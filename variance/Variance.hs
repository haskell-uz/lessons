{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall -Wextra -Wcompat -Weverything -Wno-unsafe -Wno-missing-safe-haskell-mode #-}

module Variance
  ( Covariant
  , Bifunctor
  , Contravariant
  , Profunctor
  ) where

import Data.Kind (Constraint, Type)

type Covariant :: (Type -> Type) -> Constraint
class Covariant f where
  comap :: (a -> b) -> f a -> f b

type Bifunctor :: (Type -> Type -> Type) -> Constraint
class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (c -> d) -> p a c -> p a d

type Contravariant :: (Type -> Type) -> Constraint
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

type Profunctor :: (Type -> Type -> Type) -> Constraint
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  rmap :: (c -> d) -> p a c -> p a d
