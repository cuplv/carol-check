{-# LANGUAGE LambdaCase #-}

module Language.CBPV where

import Control.Monad.Free

newtype Return v = Return v deriving (Show,Eq,Ord)

instance Functor Return where
  fmap f (Return v) = Return $ f v

data CBPV i m v =
    Fn i (m v)
  | Ap v (m v)
  | Exec (m v) i (m v)
  | Force v
  | Ite v (m v) (m v)

instance (Functor m) => Functor (CBPV i m) where
  fmap f = \case
              Fn i m -> Fn i (fmap f m)
              Ap v m -> Ap (f v) (fmap f m)
              Exec m1 i m2 -> Exec (fmap f m1) i (fmap f m2)
              Force v -> Force (f v)
              Ite v m1 m2 -> Ite (f v) (fmap f m1) (fmap f m2)

type Node i = CBPV i (Free Return)
type Term i = Free (Node i)
