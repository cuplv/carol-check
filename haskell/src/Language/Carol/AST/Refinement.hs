{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Carol.AST.Refinement
  ( Refinement (..)
  , RefDomain (..)
  , rpred
  , IVarId (..)
  ) where

import Data.SBV
import Data.Map (Map)

import Language.Carol.AST.PrettyPrint

newtype IVarId = IVarId String deriving (Show,Eq,Ord)

instance Pretty IVarId where
  pretty (IVarId s) = s


class (Eq d, Eq (DRef d), Eq (ISort d), Ord d, Ord (DRef d), Ord (ISort d))
    => RefDomain d where
  data DRef d
  data ISort d
  refConstraint :: Map IVarId SInteger -> DRef d -> SInteger -> SBool
  subVar :: IVarId -> DRef d -> DRef d

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

rpred :: (RefDomain d) => Map IVarId SInteger -> Refinement d -> SInteger -> SBool
rpred m = \case
  RefTrue -> const sTrue
  RefFalse -> const sFalse
  RefAtom dr -> refConstraint m dr
  RefAnd r1 r2 -> \nu -> rpred m r1 nu .&& rpred m r2 nu
