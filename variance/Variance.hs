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
  bifst :: (a -> b) -> p a c -> p b c
  bisnd :: (c -> d) -> p a c -> p a d

type Contravariant :: (Type -> Type) -> Constraint
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

type Profunctor :: (Type -> Type -> Type) -> Constraint
class Profunctor p where
  promap :: (b -> a) -> (c -> d) -> p a c -> p b d
  profst :: (b -> a) -> p a c -> p b c
  prosnd :: (c -> d) -> p a c -> p a d
