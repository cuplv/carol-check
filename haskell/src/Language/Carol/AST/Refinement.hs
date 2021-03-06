{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Refinement
  ( Refinement (..)
  , RefDomain (..)
  , rpred
  ) where

import Data.SBV

import Language.Carol.AST.PrettyPrint

class (Eq d, Eq (DRef d), Ord d, Ord (DRef d))
    => RefDomain d where
  data DRef d
  refConstraint :: DRef d -> SInteger -> SBool

data (RefDomain d) => Refinement d =
    RefTrue
  | RefFalse
  | RefAtom (DRef d)
  | RefAnd (Refinement d) (Refinement d)
  deriving (Eq,Ord)

deriving instance (RefDomain d, Show (DRef d))
    => Show (Refinement d)

instance (RefDomain d, Pretty (DRef d))
    => Pretty (Refinement d) where
  pretty = \case
    RefTrue -> "⊤"
    RefFalse -> "⊥"
    RefAtom r -> pretty r
    RefAnd r1 r2 -> pretty r1 ++ " ∧ " ++ pretty r2

rpred :: (RefDomain d) => Refinement d -> SInteger -> SBool
rpred = \case
  RefTrue -> const sTrue
  RefFalse -> const sFalse
  RefAtom dr -> refConstraint dr
  RefAnd r1 r2 -> \nu -> rpred r1 nu .&& rpred r2 nu
