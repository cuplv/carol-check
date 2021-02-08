{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Refinement
  ( AnySym (..)
  , Refinement (..)
  , RefDomain (..)
  , rpred
  , IVarId
  ) where

import Data.SBV
import Data.Map (Map)

import Language.Carol.Prelude.Types

data (RefDomain d) => AnySym d = RSym (DRSym d)

class (Eq d, Eq (DRef d), Eq (ISort d), Ord d, Ord (DRef d), Ord (ISort d))
    => RefDomain d where
  data DRef d
  data ISort d
  data DRSym d
  refConstraint :: Map IVarId (AnySym d) -> DRef d -> AnySym d -> SBool
  subVar :: IVarId -> DRef d -> DRef d
  mkSym :: d -> SymbolicT IO (AnySym d)

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

rpred :: (RefDomain d) => Map IVarId (AnySym d) -> Refinement d -> AnySym d -> SBool
rpred m = \case
  RefTrue -> const sTrue
  RefFalse -> const sFalse
  RefAtom dr -> refConstraint m dr
  RefAnd r1 r2 -> \nu -> rpred m r1 nu .&& rpred m r2 nu
